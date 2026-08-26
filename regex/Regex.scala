package halotukozak.regex

import halotukozak.commons.deepRecursive

import scala.annotation.{threadUnsafe, unused}
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, ToExpr, Varargs}
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
 * `(...)`, lookahead `(?=...)` `(?!...)`, the `i` inline flag `(?i)` `(?-i)` `(?i:...)`
 * `(?-i:...)`, anchors `^` `$` `\A` `\Z` `\z`, quantifiers `*` `+` `?` `{n}` `{n,}` `{n,m}`
 * (bounds capped at [[Regex.maxRepeatBound]]).
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

  /**
   * Bounded repetition `r{lo,hi}` (`hi` may be `Int.MaxValue` for the unbounded `{lo,}` form).
   * Kept as a single node carrying `lo`/`hi` symbolically, instead of unfolding into that many
   * copies of `r` - see [[Regex.repeat]] for why, and [[Subset.rawDerive]] for how a Brzozowski
   * derivative is taken through it without ever materializing the unfolded form.
   */
  case Repeat private[Regex] (r: Regex, lo: Int, hi: Int)

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
    case Repeat(r, lo, _) => lo == 0 || r.nullable
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
    case Repeat(inner, _, _) => inner.alphabetBoundaries
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
    case Repeat(inner, _, _) => inner.hasStartAnchor
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

  /**
   * Quantifier `this{lo,hi}` where `hi` can be `Int.MaxValue` for unbounded. Produces a single
   * [[Repeat]] node carrying `lo`/`hi` symbolically - not `lo`/`hi` copies of `this` - the way
   * `regex-syntax` (Rust's regex crate) keeps counted repetitions unexpanded in its `Hir` so AST
   * size stays proportional to the pattern's *text*, not the language it denotes; a `{1,1000}`
   * quantifier used to mean ~1000 [[Concat]]/[[Alt]] nodes here too, which was both wasted
   * allocation on every call and, when spliced by `regex"..."`'s `ToExpr[Regex]`, deep enough to
   * overflow the compiler's own stack expanding the macro.
   */
  def repeat(lo: Int, hi: Int): Regex =
    require(lo >= 0 && hi >= lo, s"invalid bounds {$lo,$hi}")
    require(
      lo <= Regex.maxRepeatBound && (hi == Int.MaxValue || hi <= Regex.maxRepeatBound),
      s"quantifier bound exceeds maximum supported value of ${Regex.maxRepeatBound} (got {$lo,$hi})",
    )
    this match
      case _ if hi == 0 => Regex.Eps
      case Regex.Eps => Regex.Eps
      case Regex.Empty => if lo == 0 then Regex.Eps else Regex.Empty
      case _ if lo == 0 && hi == Int.MaxValue => star
      case _ if lo == 1 && hi == 1 => this
      case _ => Regex.Repeat(this, lo, hi)

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
   * Upper bound on quantifier bounds accepted by [[repeat]]. `Repeat` itself stays O(1)
   * regardless of `lo`/`hi` (see [[repeat]]), but each Brzozowski derivative step through it
   * (`Subset.rawDerive`) peels one repetition off, so exploring `L(r{0,hi})` up to a fixed point
   * (`Subset.isEmpty`/`subset`) still visits on the order of `hi` distinct states - an unbounded
   * value would let a single malformed pattern blow up that exploration. Real-world token
   * patterns never need bounds anywhere near this size.
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
  /**
   * A pending step in [[ToExpr]]'s iterative post-order walk: index `node`'s children first
   * (`Enter`), then, once they're all indexed, `node` itself (`Exit`).
   */
  private enum ExprBuildStep:
    case Enter(node: Regex)
    case Exit(node: Regex)

  inline private val EpsTag = 0
  inline private val EmptyTag = 1
  inline private val CharsTag = 2
  inline private val ConcatTag = 3
  inline private val AltTag = 4
  inline private val InterTag = 5
  inline private val StarTag = 6
  inline private val ComplTag = 7
  inline private val LookTag = 8
  inline private val StartAnchorTag = 9
  inline private val RepeatTag = 10

  /**
   * Reconstructs a `Regex` from the compact string [[ToExpr]] produces: `nodes|partsFlat`, where
   * `nodes` is a `,`-joined `tag,arg1,arg2,arg3` quadruple per node (nodes laid out so every
   * child index is smaller than its parent's) and `partsFlat` is a `,`-joined flat list of
   * [[Alt]]/[[Inter]] children indices. A single forward pass then builds each node from its
   * already-built children, with no recursion at all on either side of this round trip.
   *
   * `arg1`/`arg2`/`arg3` are reused across tags rather than given one array per case, since at
   * most three ints are ever needed per node:
   *  - [[Concat]]: `arg1`/`arg2` = index of `a`/`b`.
   *  - [[Star]]/[[Compl]]: `arg1` = index of the inner `r`.
   *  - [[Look]]: `arg1` = index of `r`; `arg2` = `positive` as `0`/`1`.
   *  - [[Repeat]]: `arg1` = index of `r`; `arg2`/`arg3` = `lo`/`hi`.
   *  - [[Chars]]: `arg1` = index into `charSets`.
   *  - [[Alt]]/[[Inter]]: `arg1`/`arg2` = offset/count of this node's children within `partsFlat`.
   *
   * A single string, not the five parallel `Array[Int]` literals an earlier version of this used,
   * because each element of an embedded array literal costs its own `dup`/`ldc`/`iastore`
   * bytecode instructions - enough of them (e.g. from a long literal string; see [[ToExpr]]) hits
   * the JVM's 64KB-per-method bytecode ceiling. A string constant is one `ldc` regardless of
   * length, so the call site's bytecode size no longer scales with the source pattern's size at
   * all; the only remaining limit is the classfile format's 65535-byte cap on a single constant.
   */
  private[regex] def fromEncoded(encoded: String, charSets: IndexedSeq[CharSet]): Regex =
    val bar = encoded.indexOf('|')
    val nodeInts = encoded.substring(0, bar).split(',').map(_.toInt)
    val partsFlatStr = encoded.substring(bar + 1)
    val partsFlat = if partsFlatStr.isEmpty then Array.empty[Int] else partsFlatStr.split(',').map(_.toInt)

    val n = nodeInts.length / 4
    val nodes = new Array[Regex](n)
    var i = 0
    while i < n do
      val tag = nodeInts(i * 4)
      val arg1 = nodeInts(i * 4 + 1)
      val arg2 = nodeInts(i * 4 + 2)
      val arg3 = nodeInts(i * 4 + 3)
      nodes(i) = tag match
        case EpsTag => Eps
        case EmptyTag => Empty
        case StartAnchorTag => StartAnchor
        case CharsTag => Chars(charSets(arg1))
        case ConcatTag => Concat(nodes(arg1), nodes(arg2))
        case StarTag => Star(nodes(arg1))
        case ComplTag => Compl(nodes(arg1))
        case LookTag => Look(nodes(arg1), arg2 != 0)
        case RepeatTag => Repeat(nodes(arg1), arg2, arg3)
        case AltTag | InterTag =>
          var parts = Set.empty[Regex]
          var k = 0
          while k < arg2 do
            parts += nodes(partsFlat(arg1 + k))
            k += 1
          if tag == AltTag then Alt(parts) else Inter(parts)
      i += 1
    nodes(n - 1)

  /**
   * Embeds the value's already-computed shape as a compact string reconstructed by
   * [[fromEncoded]], instead of regenerating it through the public smart constructors (`concat`,
   * `alt`, `inter`, `star`, `unary_!`, `lookahead`) at every call site - the same spirit as
   * [[TokenMatcher]]'s `ToExpr` embedding its DFA tables as plain data. The smart constructors
   * exist to (re-)establish ACI normalization and merge `Chars` sets when building a tree from
   * scratch; a `Regex` value reaching this `given` is already normalized, so re-running them at
   * every macro call site - and again at every class load, since the generated code re-executes
   * them - would just redo that work for a result that's already known.
   *
   * Splicing raw constructor calls one-per-node instead (as an earlier version of this `given`
   * did) still isn't enough on its own: `regex"..."` on a pattern producing a sufficiently deep
   * tree - not just from a `{lo,hi}` quantifier (see [[Repeat]], which now keeps those O(1)), but
   * e.g. from a long literal string, since [[literal]] still builds one [[Concat]] node per
   * character - can `StackOverflowError` the *compiler itself* while it walks that many levels
   * of nested calls in a later phase (macro splicing, inlining), regardless of how carefully
   * *this* macro was written. Embedding as a single string is a single call with no nesting, so
   * no compiler pass has a deep tree to walk no matter how deep the source `Regex` is - see
   * [[fromEncoded]] for why a string beats parallel `Array[Int]` literals for that same call
   * site's own bytecode size, which was still an (orthogonal, JVM-classfile-level) limit on a
   * long enough pattern even after this fix's first version closed the stack-overflow.
   *
   * Both directions avoid recursion entirely: encoding below walks the tree with an explicit
   * `steps` stack (needed because, unlike [[halotukozak.commons.deepRecursive]]-trampolined
   * `concat`/`derive`, [[Alt]]/[[Inter]] recurse into a `Set` of unbounded size, which that
   * machinery can't unroll), and [[fromEncoded]]'s reconstruction is a single forward loop.
   *
   * `index` doubles as a dedup cache: a subtree reachable from multiple places in the tree (e.g.
   * a repeated alternative) is encoded, and later reconstructed, only once.
   */
  given ToExpr[Regex]:
    def apply(r: Regex)(using Quotes): Expr[Regex] =
      import ExprBuildStep.*
      val index = mutable.Map.empty[Regex, Int]
      val tags = mutable.ArrayBuffer.empty[Int]
      val arg1 = mutable.ArrayBuffer.empty[Int]
      val arg2 = mutable.ArrayBuffer.empty[Int]
      val arg3 = mutable.ArrayBuffer.empty[Int]
      val partsFlat = mutable.ArrayBuffer.empty[Int]
      val charSets = mutable.ArrayBuffer.empty[CharSet]
      val steps = mutable.ArrayDeque(Enter(r))

      def push(tag: Int, a: Int, b: Int, c: Int): Int =
        val idx = tags.length
        tags += tag
        arg1 += a
        arg2 += b
        arg3 += c
        idx

      def enter(node: Regex): Unit = if !index.contains(node) then steps += Enter(node)

      while steps.nonEmpty do
        steps.removeLast() match
          case Enter(node) if index.contains(node) => ()
          case Enter(node) =>
            node match
              case Eps => index(node) = push(EpsTag, 0, 0, 0)
              case Empty => index(node) = push(EmptyTag, 0, 0, 0)
              case StartAnchor => index(node) = push(StartAnchorTag, 0, 0, 0)
              case Chars(set) =>
                val csIdx = charSets.length
                charSets += set
                index(node) = push(CharsTag, csIdx, 0, 0)
              case Concat(a, b) =>
                steps += Exit(node)
                enter(b)
                enter(a)
              case Star(inner) =>
                steps += Exit(node)
                enter(inner)
              case Repeat(inner, _, _) =>
                steps += Exit(node)
                enter(inner)
              case Compl(inner) =>
                steps += Exit(node)
                enter(inner)
              case Look(inner, _) =>
                steps += Exit(node)
                enter(inner)
              case Alt(parts) =>
                steps += Exit(node)
                parts.foreach(enter)
              case Inter(parts) =>
                steps += Exit(node)
                parts.foreach(enter)
          case Exit(node) =>
            index(node) = node match
              case Concat(a, b) => push(ConcatTag, index(a), index(b), 0)
              case Star(inner) => push(StarTag, index(inner), 0, 0)
              case Repeat(inner, lo, hi) => push(RepeatTag, index(inner), lo, hi)
              case Compl(inner) => push(ComplTag, index(inner), 0, 0)
              case Look(inner, positive) => push(LookTag, index(inner), if positive then 1 else 0, 0)
              case Alt(parts) =>
                val off = partsFlat.length
                parts.foreach(p => partsFlat += index(p))
                push(AltTag, off, parts.size, 0)
              case Inter(parts) =>
                val off = partsFlat.length
                parts.foreach(p => partsFlat += index(p))
                push(InterTag, off, parts.size, 0)
              case leaf => throw MatchError(s"unreachable: leaf node $leaf pushed as Exit")

      val nodesPart = Iterator
        .range(0, tags.length)
        .flatMap(i => Iterator(tags(i), arg1(i), arg2(i), arg3(i)))
        .mkString(",")
      val encoded = s"$nodesPart|${partsFlat.mkString(",")}"
      val charSetsExpr = Varargs(charSets.toSeq.map(Expr(_)))
      '{ Regex.fromEncoded(${ Expr(encoded) }, IndexedSeq($charSetsExpr*)) }
  // $COVERAGE-ON$

extension [A, CC[X] <: Iterable[X]](xs: scala.collection.IterableOps[A, CC, CC[A]])
  inline private def partitionIsInstance[T <: A]: (CC[T], CC[A]) =
    val (matches, rest) = xs.partition(_.isInstanceOf[T])
    (matches.asInstanceOf[CC[T]], rest)
