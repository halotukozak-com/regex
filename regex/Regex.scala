package halotukozak.regex

import halotukozak.commons.deepRecursive

import scala.annotation.{threadUnsafe, unused}
import scala.collection.immutable.SortedSet
import scala.quoted.{Expr, Quotes, ToExpr}
import scala.util.hashing.MurmurHash3

/**
 * The `args` parameter only exists so this satisfies the shape Scala's `StringContext`
 * interpolation desugaring requires (`sc"..."` lowers to `sc.regex(interpolatedExprs*)`); the
 * pattern itself is built purely from the string-literal parts, so splices are not supported.
 *
 * `sc` must be `inline` too: without it, the compiler binds the extension receiver to a
 * synthetic proxy val before splicing, so `'sc` inside the macro body no longer refers to the
 * literal `StringContext(...)` call and `scExpr.value` always sees `None` — silently
 * degrading every `regex"..."` call to the runtime-parsing fallback instead of validating at
 * compile time.
 */
extension (inline sc: StringContext) inline def regex(@unused args: Any*): Regex = ${ regexInterpolatorImpl('sc) }

private def regexInterpolatorImpl(scExpr: Expr[StringContext])(using quotes: Quotes): Expr[Regex] =
  import quotes.reflect.*
  scExpr.value match
    case Some(sc) =>
      val pattern = sc.parts.mkString
      RegexParser.parse(pattern) match
        case Left(error) =>
          report.errorAndAbort(s"Regex parse error: $error")
        case Right(regex) =>
          Expr(regex)
    case None =>
      '{
        RegexParser.parse(${ scExpr }.parts.mkString) match
          case Left(error) => throw IllegalArgumentException(s"Regex parse error: $error")
          case Right(regex) => regex
      }

/**
 * A symbolic regex algebra with exact language containment ("is this pattern a subset of
 * that one?") in addition to the usual construction combinators — not just string matching.
 * Pure Scala, no `java.util.regex` (or any other JVM-only API) anywhere in the implementation,
 * so it compiles and runs identically on the JVM, Scala.js, and Scala Native.
 *
 * Construct via smart factories on the companion ([[Regex.lit]], [[Regex.literal]],
 * [[Regex.range]], [[Regex.apply]], [[Regex.alt]], [[Regex.inter]], [[Regex.all]]) and
 * member combinators (`a concat b`, `a | b`, `a & b`, `!a`, `r.star`, `r.repeat(n, m)`).
 * Raw case class constructors are inaccessible — only pattern matching via the
 * generated `unapply` methods is allowed.
 *
 * Supports literals, escapes (\d \D \s \S \w \W \t \n \r \f \a \e \v \cX \0[n[n]] \xhh
 * \x{h...h} \uhhhh \Q...\E \R and meta-escapes), character classes (including ranges, negation,
 * nested subclasses, and `&&` intersection), `.`, alternation `|`, non-capturing-style groups
 * `(...)`, lookahead `(?=...)` `(?!...)`, anchors `^` `$` `\A` `\Z` `\z`, quantifiers `*` `+` `?`
 * `{n}` `{n,}` `{n,m}` (bounds capped at [[Regex.maxRepeatBound]]).
 *
 * Unsupported (parser returns [[RegexParseError.UnsupportedFeature]]):
 * word-boundary anchors `\b` `\B`, `\G`, lookbehind `(?<=` `(?<!`,
 * backreferences `\1`..`\9` `\k<name>` `\g{...}`, Unicode properties `\p{...}`, grapheme
 * clusters `\X`.
 *
 * For subset/emptiness/nullability queries and Brzozowski derivatives, see [[Subset]].
 */
enum Regex:

  /** Matches the empty string ε. */
  case Eps

  /** Matches no string. */
  case Empty

  /** Matches any single code point in `set`. */
  case Chars private[Regex] (set: CharSet)

  /** Concatenation `a · b`. */
  case Concat private[Regex] (a: Regex, b: Regex)

  /** Alternation. Stored as a [[Set]] for ACI normalization. Always size ≥ 2. */
  case Alt private[Regex] (parts: Set[Regex])

  /** Intersection. Stored as a [[Set]] for ACI normalization. Always size ≥ 2. */
  case Inter private[Regex] (parts: Set[Regex])

  /** Kleene star `r*`. */
  case Star private[Regex] (r: Regex)

  /** Complement `¬r`. */
  case Compl private[Regex] (r: Regex)

  /**
   * Zero-width lookahead assertion. `positive = true` is `(?=r)`: holds at a position iff some
   * prefix of the string remaining there is in `L(r)`, without consuming any of it.
   * `positive = false` is `(?!r)`: holds iff no such prefix exists.
   */
  case Look private[Regex] (r: Regex, positive: Boolean)

  /**
   * `^`/`\A` (start-of-input anchor). Unlike [[Look]], this can't be resolved by re-deriving a
   * sub-pattern alongside whatever follows it - it's satisfied only in the state before any
   * character has been consumed *by anything*, anywhere in the whole match, not just locally at
   * this node. See [[Subset.derive]]'s unconditional post-pass, which is what actually enforces
   * that. `$`/`\Z`/`\z` don't need an equivalent node - under this library's whole-string
   * `matches()` semantics they're all exactly `(?!.)` (negative lookahead of any code point),
   * expressed directly with [[Look]] at parse time.
   */
  case StartAnchor

  /**
   * Cached: this tree is immutable and gets hashed repeatedly by the `Set`-based ACI
   * normalization in `Regex.alt`/`Regex.inter` and by the visited-state set driving
   * Brzozowski derivative exploration in [[Subset]] — recomputing structurally every time
   * would walk the whole subtree on each lookup.
   */
  override lazy val hashCode: Int = MurmurHash3.caseClassHash(this)

  /**
   * Cached: nullability is invariant per node but gets re-queried by `Subset.deriveImpl`'s
   * `Concat` case on every derivative step (once per representative char, for every `Concat`
   * node on the path) — recomputing structurally every time would re-walk the same subtree
   * over and over during Brzozowski derivative exploration.
   */
  @threadUnsafe lazy val nullable: Boolean = this match
    case Eps => true
    case Empty | Chars(_) => false
    case Concat(a, b) => a.nullable && b.nullable
    case Alt(parts) => parts.exists(_.nullable)
    case Inter(parts) => parts.forall(_.nullable)
    case Star(_) => true
    case Compl(inner) => !inner.nullable
    case Look(r, positive) => if positive then r.nullable else !r.nullable
    case StartAnchor => true

  /**
   * Cached: sorted boundary points (range starts, and one-past-the-end of range ends) of
   * every [[Chars]] leaf reachable from this node — the raw material `Subset.partitionReps`
   * combines with the sentinels `0`/`CharSet.maxCodePoint + 1` to pick one representative
   * code point per equivalence class of the alphabet partition. This is queried once per
   * BFS state during `Subset.isEmptyImpl`/`subset`; recomputing it by re-walking the whole
   * subtree on every state (most of which share large unchanged substructures across
   * consecutive derivative steps) would repeatedly re-derive the same per-node result.
   */
  @threadUnsafe lazy val alphabetBoundaries: SortedSet[Int] = this match
    case Eps | Empty => SortedSet.empty
    case Chars(set) => SortedSet.from(set.iterator.flatMap(r => Iterator(r.lo, r.hi + 1)))
    case Concat(a, b) => a.alphabetBoundaries ++ b.alphabetBoundaries
    case Alt(parts) => parts.iterator.map(_.alphabetBoundaries).reduce(_ ++ _)
    case Inter(parts) => parts.iterator.map(_.alphabetBoundaries).reduce(_ ++ _)
    case Star(inner) => inner.alphabetBoundaries
    case Compl(inner) => inner.alphabetBoundaries
    case Look(r, _) => r.alphabetBoundaries
    case StartAnchor => SortedSet.empty

  /**
   * Cached: `true` iff `^`/`\A` occurs anywhere in this tree, including inside [[Look]] bodies
   * (a start-anchor there refers to the same global position as the `Look` node itself) and
   * [[Star]] bodies (loop iterations past the first can never be at position 0 again). Guards
   * `Subset.derive`'s post-derivation strip pass so it's a single cheap check - not a tree
   * walk - for the overwhelming majority of patterns that never use `^`/`\A` at all.
   */
  @threadUnsafe lazy val hasStartAnchor: Boolean = this match
    case StartAnchor => true
    case Eps | Empty | Chars(_) => false
    case Concat(a, b) => a.hasStartAnchor || b.hasStartAnchor
    case Alt(parts) => parts.exists(_.hasStartAnchor)
    case Inter(parts) => parts.exists(_.hasStartAnchor)
    case Star(inner) => inner.hasStartAnchor
    case Compl(inner) => inner.hasStartAnchor
    case Look(r, _) => r.hasStartAnchor

  /** Alternation: `this | other`. */
  infix def |(other: Regex): Regex = Regex.alt(Set(this, other))

  /** Intersection: `this ∩ other`. */
  infix def &(other: Regex): Regex = Regex.inter(Set(this, other))

  /** Complement: `¬this`. */
  def unary_! : Regex = this match
    case Regex.Compl(inner) => inner
    case _ => Regex.Compl(this)

  /** Kleene star: `this*`. */
  def star: Regex = this match
    case Regex.Eps | Regex.Empty => Regex.Eps
    case s: Regex.Star => s
    case _ => Regex.Star(this)

  def * : Regex = star

  /** Quantifier `this{n,m}` where m can be `Int.MaxValue` for unbounded. */
  def repeat(lo: Int, hi: Int): Regex =
    require(lo >= 0 && hi >= lo, s"invalid bounds {$lo,$hi}")
    require(
      lo <= Regex.maxRepeatBound && (hi == Int.MaxValue || hi <= Regex.maxRepeatBound),
      s"quantifier bound exceeds maximum supported value of ${Regex.maxRepeatBound} (got {$lo,$hi})",
    )
    val mandatory = (1 to lo).foldLeft[Regex](Regex.Eps)((acc, _) => this.concat(acc))
    if hi == Int.MaxValue then mandatory.concat(star)
    else mandatory.concat((1 to (hi - lo)).foldLeft[Regex](Regex.Eps)((acc, _) => Regex.Eps | this.concat(acc)))

object Regex:

  extension (a: Regex)
    infix def concat(b: Regex): Regex = deepRecursive:
      (a, b) match
        case (Empty, _) | (_, Empty) => Empty
        case (Eps, x) => x
        case (x, Eps) => x
        case (Concat(x, y), z) => Concat(x, y.concat(z))
        case _ => Concat(a, b)

  /**
   * Upper bound on quantifier bounds accepted by [[repeat]]. `{n,m}` is unfolded into an
   * `n`- (or `m`-) sized chain of [[Concat]] nodes, so unbounded values would let a single
   * malformed pattern build an unbounded/exponential AST. Real-world token patterns never
   * need bounds anywhere near this size.
   */
  val maxRepeatBound: Int = 1000

  def apply(set: CharSet): Regex = if set.isEmpty then Empty else Chars(set)

  /** Alternation of a collection. */
  def alt(parts: Iterable[Regex]): Regex =
    val flat = parts.iterator
      .flatMap:
        case Alt(p) => p
        case Empty => Iterator.empty
        case r => Iterator.single(r)
      .toSet
    if flat.isEmpty then Empty
    else if flat.sizeIs == 1 then flat.head
    else
      val (chars, rest) = flat.partitionIsInstance[Chars]
      val merged: Set[Regex] =
        if chars.sizeIs <= 1 then flat
        else
          val union = chars.iterator.map(_.set).foldLeft(CharSet.empty)(_ union _)
          if union.isEmpty then rest else rest + Chars(union)
      if merged.sizeIs == 1 then merged.head else Alt(merged)

  /** Intersection of a collection. */
  def inter(parts: Iterable[Regex]): Regex =
    val seq = parts.iterator
      .flatMap:
        case Inter(p) => p
        case r => Iterator.single(r)
      .toSet
    if seq.isEmpty || seq.contains(Empty) then Empty
    else
      val (chars, rest) = seq.partitionIsInstance[Chars]
      val merged =
        if chars.sizeIs <= 1 then seq
        else
          val isect = chars.iterator.map(_.set).reduce(_ intersect _)
          if isect.isEmpty then null else rest + Chars(isect)
      merged match
        case null => Empty
        case m if m.sizeIs == 1 => m.head
        case m => Inter(m)

  /**
   * Zero-width lookahead assertion: `Regex.lookahead(r, true)` is `(?=r)`, `Regex.lookahead(r,
   * false)` is `(?!r)`. Degenerates to [[Eps]]/[[Empty]] directly when `r` itself is
   * [[Eps]]/[[Empty]], since the assertion is then a constant regardless of context.
   */
  def lookahead(r: Regex, positive: Boolean): Regex = r match
    case Empty => if positive then Empty else Eps
    case Eps => if positive then Eps else Empty
    case _ => Look(r, positive)

  /** All-strings regex `Σ*`. */
  val all: Regex = Regex(CharSet.all).star

  /** Convenience: literal char. */
  def lit(c: Char): Regex = Regex(CharSet.single(c))

  /** Convenience: char range. */
  def range(lo: Char, hi: Char): Regex = Regex(CharSet.range(lo, hi))

  /** Convenience: literal string. */
  def literal(s: String): Regex =
    if s.isEmpty then Eps
    else s.foldRight(Eps: Regex)((c, acc) => lit(c).concat(acc))

  // $COVERAGE-OFF$
  given ToExpr[Regex]:
    def apply(r: Regex)(using Quotes): Expr[Regex] = r match
      case Eps => '{ Regex.Eps }
      case Empty => '{ Regex.Empty }
      case Chars(set) => '{ Regex(${ Expr(set) }) }
      case Concat(a, b) => '{ ${ Expr(a) }.concat(${ Expr(b) }) }
      case Alt(parts) =>
        val partsExpr = Expr.ofSeq(parts.toSeq.map(Expr(_)))
        '{ Regex.alt($partsExpr) }
      case Inter(parts) =>
        val partsExpr = Expr.ofSeq(parts.toSeq.map(Expr(_)))
        '{ Regex.inter($partsExpr) }
      case Star(inner) => '{ ${ Expr(inner) }.star }
      case Compl(inner) => '{ ! ${ Expr(inner) } }
      case Look(r, positive) => '{ Regex.lookahead(${ Expr(r) }, ${ Expr(positive) }) }
      case StartAnchor => '{ Regex.StartAnchor }
  // $COVERAGE-ON$

extension [A, CC[X] <: Iterable[X]](xs: scala.collection.IterableOps[A, CC, CC[A]])
  inline private def partitionIsInstance[T <: A]: (CC[T], CC[A]) =
    val (matches, rest) = xs.partition(_.isInstanceOf[T])
    (matches.asInstanceOf[CC[T]], rest)
