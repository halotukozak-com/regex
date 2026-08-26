package halotukozak.regex

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.ArraySeq
import scala.quoted.*
import scala.util.boundary
import scala.util.boundary.*

/**
 * Sorted, non-overlapping, merged ranges over Int code points.
 *
 * Opaque over [[ArraySeq]]`[Range]`: every instance is built through a normalizing smart
 * constructor ([[CharSet.normalize]], [[CharSet.single]], [[CharSet.range]], [[CharSet.empty]],
 * [[CharSet.all]], [[CharSet.of]]) - `complement`/`intersect` rely on that invariant.
 * `ArraySeq`'s true O(1) array-backed indexed access
 * (`contains`'s binary search, `intersect`/`complement`'s indexed loops) and structural
 * `equals`/`hashCode` (which `Regex`'s `Set`-based ACI normalization and cached `hashCode`
 * rely on) come along for free from the underlying representation.
 */
opaque type CharSet = ArraySeq[Range]

object CharSet:

  /** Lifts an already-normalized [[ArraySeq]] of ranges into a [[CharSet]]. */
  def of(ranges: ArraySeq[Range]): CharSet = ranges

  extension (cs: CharSet)
    def contains(c: Int): Boolean =
      @tailrec
      def loop(lo: Int, hi: Int): Boolean =
        if lo > hi then false
        else
          val mid = (lo + hi) >>> 1
          val r = cs(mid)
          if c < r.lo then loop(lo, mid - 1)
          else if c > r.hi then loop(mid + 1, hi)
          else true
      loop(0, cs.length - 1)

    def isEmpty: Boolean = cs.isEmpty

    infix def union(other: CharSet): CharSet =
      val builder = ArraySeq.newBuilder[Range]
      builder.sizeHint(cs.length + other.length)
      var i = 0
      var j = 0
      var cur: Range | Null = null
      while i < cs.length || j < other.length do
        val takeFromA = j >= other.length || (i < cs.length && cs(i).lo <= other(j).lo)
        val next = if takeFromA then cs(i) else other(j)
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
      of(builder.result())

    /**
     * `cs.union(other)` here would make the compiler's overload search also consider (and
     * silently bind to instead of `union` above) the inherited, deprecated `SeqOps.union` -
     * `CharSet` is transparently `ArraySeq[Range]` in this defining scope. Renaming the import
     * forces resolution to *this* `union` specifically, since `unionRenamed` has nothing else
     * to collide with. `@nowarn` on the whole method (rather than the `import` line itself,
     * which doesn't accept annotations) silences the resulting - and, here, incorrect -
     * "unused import" warning: the renamed import *is* used, just via extension-call syntax
     * (`cs.unionRenamed(...)`) the unused-import checker doesn't recognize as a use.
     */
    @nowarn("msg=unused")
    def |(other: CharSet): CharSet =
      import CharSet.union as unionRenamed
      cs.unionRenamed(other)

    infix def intersect(other: CharSet): CharSet =
      // No sizeHint here (unlike union/normalize): the actual intersection is very often much
      // smaller than min(a.length, b.length) - e.g. near-empty for disjoint or
      // barely-overlapping sets, a common case (`[a-z] & [A-Z]`) - so hinting that upper bound
      // pre-allocates array capacity that's usually mostly wasted, which benchmarked as a net
      // loss versus just letting the builder grow on demand.
      val builder = ArraySeq.newBuilder[Range]
      var i = 0
      var j = 0
      while i < cs.length && j < other.length do
        val x = cs(i)
        val y = other(j)
        val lo = math.max(x.lo, y.lo)
        val hi = math.min(x.hi, y.hi)
        if lo <= hi then builder += Range(lo, hi)
        if x.hi < y.hi then i += 1 else j += 1
      of(builder.result())

    /**
     * Same reasoning as `|` above - `SeqOps` also inherits an `intersect` (multiset
     * intersection by element equality, not numeric overlap).
     */
    @nowarn("msg=unused")
    def &(other: CharSet): CharSet =
      import CharSet.intersect as intersectRenamed
      cs.intersectRenamed(other)

    def complement: CharSet =
      // Walks `cs` by index rather than `.head`/`.tail`: unlike `Vector`, slicing off the
      // head of an array-backed `ArraySeq` is O(n) (a fresh array copy), which would make this
      // loop O(n^2) overall instead of O(n).
      val builder = ArraySeq.newBuilder[Range]
      var idx = 0
      var prev = 0
      while idx < cs.length do
        val head = cs(idx)
        if prev <= head.lo - 1 then builder += Range(prev, head.lo - 1)
        prev = head.hi + 1
        idx += 1
      if prev <= CharSet.maxCodePoint then builder += Range(prev, CharSet.maxCodePoint)
      of(builder.result())

    def iterator: Iterator[Range] = cs.iterator

  /** Upper bound used for complement. Covers all valid Unicode code points. */
  val maxCodePoint: Int = 0x10ffff

  val empty: CharSet = of(ArraySeq.empty)
  val all: CharSet = of(ArraySeq(Range(0, maxCodePoint)))

  /** What `.` matches in Java regex without DOTALL flag — all code points except line terminators. */
  val dotDefault: CharSet = normalize(
    Range(0, '\n'.toInt - 1),
    Range('\n'.toInt + 1, '\r'.toInt - 1),
    Range('\r'.toInt + 1, 0x84),
    Range(0x86, 0x2027),
    Range(0x202a, maxCodePoint),
  )

  def single(c: Char): CharSet = single(c.toInt)
  def single(c: Int): CharSet = of(ArraySeq(Range(c, c)))
  def range(lo: Char, hi: Char): CharSet = range(lo.toInt, hi.toInt)
  def range(lo: Int, hi: Int): CharSet = of(ArraySeq(Range(lo, hi)))

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
    of(builder.result())

  def normalize(ranges: Range*): CharSet = normalize(ranges)

  // $COVERAGE-OFF$
  given ToExpr[CharSet]:
    def apply(s: CharSet)(using Quotes): Expr[CharSet] =
      val rangeExprs: Seq[Expr[Range]] = s.iterator.map(Expr.apply).toSeq
      '{ CharSet.of(ArraySeq(${ Varargs(rangeExprs) }*)) }

  given FromExpr[CharSet]:
    private def fromRanges(exprs: Seq[Expr[Range]])(using Quotes): Option[CharSet] = boundary:
      Some:
        exprs.iterator
          .map:
            case Expr(range) => range
            case _ => break(None)
          .to(ArraySeq)

    override def unapply(s: Expr[CharSet])(using Quotes): Option[CharSet] = s match
      case '{ CharSet.of(ArraySeq(${ Varargs(rangeExprs) }*)) } => fromRanges(rangeExprs)
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
