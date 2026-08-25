package halotukozak.regex

import halotukozak.commons.deepRecursive

import scala.annotation.{publicInBinary, tailrec, threadUnsafe, unused}
import scala.collection.immutable.{ArraySeq, SortedSet}
import scala.quoted.{Expr, FromExpr, Quotes, ToExpr, Varargs}
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
 * `(...)`, quantifiers `*` `+` `?` `{n}` `{n,}` `{n,m}` (bounds capped at
 * [[Regex.maxRepeatBound]]).
 *
 * Unsupported (parser returns [[RegexParseError.UnsupportedFeature]]):
 * anchors `^` `$` `\b` `\B` `\A` `\Z` `\z` `\G`, lookaround `(?=` `(?!` `(?<=` `(?<!`,
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
  // $COVERAGE-ON$

/**
 * Sorted, non-overlapping, merged ranges over Int code points.
 *
 * The type itself is public - `given ToExpr[Regex]` needs to name it in code spliced into
 * arbitrary external packages by macros - but the primary constructor is restricted to
 * [[CharSet]]'s own companion so every instance is still built through a normalizing smart
 * constructor ([[CharSet.normalize]], [[CharSet.single]], [[CharSet.range]], [[CharSet.empty]],
 * [[CharSet.all]]); `complement` and `intersect` rely on that invariant.
 *
 * Backed by [[ArraySeq]] rather than `Vector`: this type is immutable once built (every
 * instance goes through the normalizing smart constructors above, never incremental
 * append/prepend), so there's nothing to gain from `Vector`'s structural sharing - but
 * `contains`'s binary search, and `intersect`/`complement`'s indexed loops, benefit from
 * `ArraySeq`'s true O(1) array-backed indexed access (`Vector`'s is O(log32 n)) while still
 * getting structural `equals`/`hashCode` for free (unlike a raw `Array`), which `Regex`'s
 * `Set`-based ACI normalization and cached `hashCode` rely on.
 */
final class CharSet @publicInBinary private[CharSet] (private val ranges: ArraySeq[Range]) extends AnyVal:

  def contains(c: Int): Boolean =
    @tailrec
    def loop(lo: Int, hi: Int): Boolean =
      if lo > hi then false
      else
        val mid = (lo + hi) >>> 1
        val r = ranges(mid)
        if c < r.lo then loop(lo, mid - 1)
        else if c > r.hi then loop(mid + 1, hi)
        else true
    loop(0, ranges.length - 1)

  def isEmpty: Boolean = ranges.isEmpty

  /**
   * Two-pointer merge of the two already-sorted, already-coalesced range sequences, O(n+m) -
   * as opposed to flattening both into one list and re-sorting from scratch (what
   * `CharSet.normalize(ranges ++ other.ranges)` used to do here, at O((n+m) log(n+m))), which
   * throws away the fact that both inputs are already ordered. Mirrors `intersect`'s existing
   * two-pointer walk, just coalescing overlapping/adjacent ranges (`normalize`'s rule) instead
   * of intersecting them.
   *
   * Writes straight into an `ArraySeq.newBuilder`, unlike `intersect`/`complement`/`normalize`
   * which accumulate into a `Vector` and convert via `ArraySeq.from` at the end - skips both
   * the `Vector` node allocations and that final copy.
   */
  infix def union(other: CharSet): CharSet =
    val builder = ArraySeq.newBuilder[Range]
    builder.sizeHint(ranges.length + other.ranges.length)
    var i = 0
    var j = 0
    var cur: Range | Null = null
    while i < ranges.length || j < other.ranges.length do
      val takeFromA = j >= other.ranges.length || (i < ranges.length && ranges(i).lo <= other.ranges(j).lo)
      val next = if takeFromA then ranges(i) else other.ranges(j)
      if takeFromA then i += 1 else j += 1
      cur match
        case null => cur = next
        case c if next.lo <= c.hi + 1 => cur = Range(c.lo, math.max(c.hi, next.hi))
        case c =>
          builder += c
          cur = next
    cur match
      case null => ()
      case c => builder += c
    CharSet(builder.result())
  def |(other: CharSet): CharSet = union(other)

  infix def intersect(other: CharSet): CharSet =
    // No sizeHint here (unlike union/normalize): the actual intersection is very often much
    // smaller than min(ranges.length, other.ranges.length) - e.g. near-empty for disjoint or
    // barely-overlapping sets, a common case (`[a-z] & [A-Z]`) - so hinting that upper bound
    // pre-allocates array capacity that's usually mostly wasted, which benchmarked as a net
    // loss versus just letting the builder grow on demand.
    val builder = ArraySeq.newBuilder[Range]
    var i = 0
    var j = 0
    while i < ranges.length && j < other.ranges.length do
      val a = ranges(i)
      val b = other.ranges(j)
      val lo = math.max(a.lo, b.lo)
      val hi = math.min(a.hi, b.hi)
      if lo <= hi then builder += Range(lo, hi)
      if a.hi < b.hi then i += 1 else j += 1
    CharSet(builder.result())

  def &(other: CharSet): CharSet = intersect(other)

  def complement: CharSet =
    // Walks `ranges` by index rather than `.head`/`.tail`: unlike `Vector`, slicing off the
    // head of an array-backed `ArraySeq` is O(n) (a fresh array copy), which would make this
    // loop O(n^2) overall instead of O(n).
    val builder = ArraySeq.newBuilder[Range]
    var idx = 0
    var prev = 0
    while idx < ranges.length do
      val head = ranges(idx)
      if prev <= head.lo - 1 then builder += Range(prev, head.lo - 1)
      prev = head.hi + 1
      idx += 1
    if prev <= CharSet.maxCodePoint then builder += Range(prev, CharSet.maxCodePoint)
    CharSet(builder.result())

  def iterator: Iterator[Range] = ranges.iterator

object CharSet:

  /** Upper bound used for complement. Covers all valid Unicode code points. */
  val maxCodePoint: Int = 0x10ffff

  val empty: CharSet = CharSet(ArraySeq.empty)
  val all: CharSet = CharSet(ArraySeq(Range(0, maxCodePoint)))

  /** What `.` matches in Java regex without DOTALL flag — all code points except line terminators. */
  val dotDefault: CharSet = normalize(
    Range(0, '\n'.toInt - 1),
    Range('\n'.toInt + 1, '\r'.toInt - 1),
    Range('\r'.toInt + 1, 0x84),
    Range(0x86, 0x2027),
    Range(0x202a, maxCodePoint),
  )

  def single(c: Char): CharSet = single(c.toInt)
  def single(c: Int): CharSet = CharSet(ArraySeq(Range(c, c)))
  def range(lo: Char, hi: Char): CharSet = range(lo.toInt, hi.toInt)
  def range(lo: Int, hi: Int): CharSet = CharSet(ArraySeq(Range(lo, hi)))

  /** Builds a normalized [[CharSet]] from arbitrary (possibly overlapping) ranges. */
  def normalize(rs: Iterable[Range]): CharSet =
    val sorted = rs.toArray
    sorted.sortInPlaceBy(_.lo)
    val builder = ArraySeq.newBuilder[Range]
    builder.sizeHint(sorted.length)
    var cur: Range | Null = null
    var i = 0
    while i < sorted.length do
      val r = sorted(i)
      cur match
        case null => cur = r
        case c if r.lo <= c.hi + 1 => cur = Range(c.lo, math.max(c.hi, r.hi))
        case c =>
          builder += c
          cur = r
      i += 1
    cur match
      case null => ()
      case c => builder += c
    CharSet(builder.result())

  def normalize(ranges: Range*): CharSet = normalize(ranges)

  // $COVERAGE-OFF$
  given ToExpr[CharSet]:
    def apply(s: CharSet)(using Quotes): Expr[CharSet] =
      val rangeExprs: Seq[Expr[Range]] = s.ranges.map(Expr.apply)
      '{ CharSet(ArraySeq(${ Varargs(rangeExprs) }*)) }

  given FromExpr[CharSet]:
    private def fromRanges(exprs: Seq[Expr[Range]])(using Quotes): Option[CharSet] =
      val builder = ArraySeq.newBuilder[Range]
      builder.sizeHint(exprs.length)
      val it = exprs.iterator
      var ok = true
      while ok && it.hasNext do
        it.next() match
          case Expr(range) => builder += range
          case _ => ok = false
      if ok then Some(new CharSet(builder.result())) else None

    override def unapply(s: Expr[CharSet])(using Quotes): Option[CharSet] = s match
      case '{ CharSet(ArraySeq(${ Varargs(rangeExprs) }*)) } => fromRanges(rangeExprs)
      case '{ new CharSet(ArraySeq(${ Varargs(rangeExprs) }*)) } => fromRanges(rangeExprs)
      case '{ CharSet.single(${ Expr(c) }: Char) } => Some(CharSet.single(c))
      case '{ CharSet.single(${ Expr(c) }: Int) } => Some(CharSet.single(c))
      case '{ CharSet.range(${ Expr(lo) }: Char, ${ Expr(hi) }: Char) } => Some(CharSet.range(lo, hi))
      case '{ CharSet.range(${ Expr(lo) }: Int, ${ Expr(hi) }: Int) } => Some(CharSet.range(lo, hi))
      case _ => None

// $COVERAGE-ON$

/** Single closed code-point range. */
final case class Range(lo: Int, hi: Int):
  require(0 <= lo && lo <= hi && hi <= CharSet.maxCodePoint, s"invalid code-point range [$lo, $hi]")

  def this(lo: Char, hi: Char) = this(lo.toInt, hi.toInt)

  def contains(c: Int): Boolean = c >= lo && c <= hi
object Range:
  // $COVERAGE-OFF$
  given ToExpr[Range]:
    def apply(x: Range)(using Quotes): Expr[Range] =
      '{ Range(${ Expr(x.lo) }, ${ Expr(x.hi) }) }

  given FromExpr[Range]:
    override def unapply(x: Expr[Range])(using Quotes): Option[Range] = x match
      case '{ Range(${ Expr(lo) }: Char, ${ Expr(hi) }: Char) } => Some(Range(lo, hi))
      case '{ Range(${ Expr(lo) }: Int, ${ Expr(hi) }: Int) } => Some(Range(lo, hi))
      case '{ new Range(${ Expr(lo) }: Char, ${ Expr(hi) }: Char) } => Some(Range(lo, hi))
      case '{ new Range(${ Expr(lo) }: Int, ${ Expr(hi) }: Int) } => Some(Range(lo, hi))
      case _ => None
  // $COVERAGE-ON$

extension [A, CC[X] <: Iterable[X]](xs: scala.collection.IterableOps[A, CC, CC[A]])
  inline private def partitionIsInstance[T <: A]: (CC[T], CC[A]) =
    val (matches, rest) = xs.partition(_.isInstanceOf[T])
    (matches.asInstanceOf[CC[T]], rest)
