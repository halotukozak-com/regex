package halotukozak.regex

import scala.annotation.{publicInBinary, tailrec, threadUnsafe}
import scala.quoted.{Expr, Quotes, ToExpr, Varargs}
import scala.util.control.TailCalls.{done, tailcall, TailRec}
import scala.util.hashing.MurmurHash3

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
 * \x{h...h} \uhhhh \Q...\E \R and meta-escapes), character classes (including ranges and
 * negation), `.`, alternation `|`, non-capturing-style groups `(...)`, quantifiers `*` `+`
 * `?` `{n}` `{n,}` `{n,m}` (bounds capped at [[Regex.maxRepeatBound]]).
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

  /** Concatenation: `this · other`. */
  infix def concat(other: Regex): Regex =
    def loop(a: Regex, b: Regex): TailRec[Regex] = (a, b) match
      case (Empty, _) | (_, Empty) => done(Empty)
      case (Eps, x) => done(x)
      case (x, Eps) => done(x)
      case (Concat(x, y), z) => tailcall(loop(y, z)).map(Concat(x, _))
      case _ => done(Concat(a, b))
    loop(this, other).result

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

  def * = star

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
 */
final class CharSet @publicInBinary private[CharSet] (val ranges: Vector[Range]) extends AnyVal:

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

  infix def union(other: CharSet): CharSet = CharSet.normalize(ranges ++ other.ranges)
  def |(other: CharSet): CharSet = union(other)

  infix def intersect(other: CharSet): CharSet =
    @tailrec
    def loop(i: Int, j: Int, acc: Vector[Range]): Vector[Range] =
      if i >= ranges.length || j >= other.ranges.length then acc
      else
        val a = ranges(i)
        val b = other.ranges(j)
        val lo = math.max(a.lo, b.lo)
        val hi = math.min(a.hi, b.hi)
        val acc1 = if lo <= hi then acc :+ Range(lo, hi) else acc
        if a.hi < b.hi then loop(i + 1, j, acc1) else loop(i, j + 1, acc1)
    CharSet(loop(0, 0, Vector.empty))

  def &(other: CharSet): CharSet = intersect(other)

  def complement: CharSet =
    @tailrec
    def loop(rs: Vector[Range], prev: Int, acc: Vector[Range]): CharSet =
      if rs.isEmpty then
        if prev <= CharSet.maxCodePoint then CharSet(acc :+ Range(prev, CharSet.maxCodePoint)) else CharSet(acc)
      else
        val head = rs.head
        val acc1 = if prev <= head.lo - 1 then acc :+ Range(prev, head.lo - 1) else acc
        loop(rs.tail, head.hi + 1, acc1)
    loop(ranges, 0, Vector.empty)

object CharSet:

  /** Upper bound used for complement. Covers all valid Unicode code points. */
  val maxCodePoint: Int = 0x10ffff

  val empty: CharSet = CharSet(Vector.empty)
  val all: CharSet = CharSet(Vector(Range(0, maxCodePoint)))

  /** What `.` matches in Java regex without DOTALL flag — all code points except line terminators. */
  val dotDefault: CharSet = normalize(
    Range(0, '\n'.toInt - 1),
    Range('\n'.toInt + 1, '\r'.toInt - 1),
    Range('\r'.toInt + 1, 0x84),
    Range(0x86, 0x2027),
    Range(0x202a, maxCodePoint),
  )

  def single(c: Char): CharSet = single(c.toInt)
  def single(c: Int): CharSet = CharSet(Vector(Range(c, c)))
  def range(lo: Char, hi: Char): CharSet = range(lo.toInt, hi.toInt)
  def range(lo: Int, hi: Int): CharSet = CharSet(Vector(Range(lo, hi)))

  /** Builds a normalized [[CharSet]] from arbitrary (possibly overlapping) ranges. */
  def normalize(rs: Iterable[Range]): CharSet =
    val (acc, last) = rs.toVector
      .sortBy(_.lo)
      .foldLeft((Vector.empty[Range], null: Range | Null)):
        case ((acc, null), r) => (acc, r)
        case ((acc, cur: Range), r) =>
          if r.lo <= cur.hi + 1 then (acc, Range(cur.lo, math.max(cur.hi, r.hi)))
          else (acc :+ cur, r)
    CharSet(last match
      case null => acc
      case r => acc :+ r)

  def normalize(ranges: Range*): CharSet = normalize(ranges)

  // $COVERAGE-OFF$
  given ToExpr[CharSet]:
    def apply(s: CharSet)(using Quotes): Expr[CharSet] =
      val rangeExprs: Seq[Expr[Range]] = s.ranges.map(Expr.apply)
      '{ CharSet(Vector(${ Varargs(rangeExprs) }*)) }
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
  // $COVERAGE-ON$

extension [A, CC[X] <: Iterable[X]](xs: scala.collection.IterableOps[A, CC, CC[A]])
  inline private def partitionIsInstance[T <: A]: (CC[T], CC[A]) =
    val (matches, rest) = xs.partition(_.isInstanceOf[T])
    (matches.asInstanceOf[CC[T]], rest)
