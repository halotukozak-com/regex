package halotukozak.regex

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/**
 * `CharSet.contains` currently does a linear scan over sorted, non-overlapping ranges — a
 * candidate for a binary search. `rangeCount` lets that (and the other set operations) be
 * measured across input sizes instead of at one fixed point.
 *
 * Run: `scala-cli run --jmh . -- CharSetBenchmark` (see `bench/README.md`).
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class CharSetBenchmark:

  @Param(Array("50", "500", "5000"))
  var rangeCount: Int = uninitialized

  private var ranges: CharSet = uninitialized
  private var interleaved: CharSet = uninitialized
  private var reversedRanges: Vector[Range] = uninitialized
  private var lastLo: Int = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    ranges = CharSet.normalize((0 until rangeCount).map(i => Range(i * 4, i * 4 + 1)))
    // Every other gap, so union/intersect actually have interleaving work to do.
    interleaved = CharSet.normalize((0 until rangeCount).map(i => Range(i * 4 + 2, i * 4 + 3)))
    reversedRanges = (0 until rangeCount).map(i => Range(i * 4, i * 4 + 1)).reverse.toVector
    // The last input range's lo, derived from the same `i * 4` formula used to build `ranges`
    // above rather than read back off `ranges` itself: this file also runs, unmodified, against
    // main's `regex/` sources as the baseline half of the CI benchmark comparison job, so it
    // can't depend on `CharSet` exposing anything beyond what main's version already does.
    lastLo = (rangeCount - 1) * 4

  /** Worst case for a linear scan: the match is the very last range. */
  @Benchmark
  def containsNearEnd(): Boolean = ranges.contains(lastLo)

  /** Best case for a linear scan: the match is the very first range. */
  @Benchmark
  def containsNearStart(): Boolean = ranges.contains(0)

  /** A miss (falls in a gap), which still has to prove absence across the whole set. */
  @Benchmark
  def containsMiss(): Boolean = ranges.contains(lastLo + 2)

  @Benchmark
  def union(): CharSet = ranges.union(interleaved)

  @Benchmark
  def intersect(): CharSet = ranges.intersect(interleaved)

  @Benchmark
  def complement(): CharSet = ranges.complement

  /** Sort-and-merge from scratch, worst-case (fully reversed) input order. */
  @Benchmark
  def normalize(): CharSet = CharSet.normalize(reversedRanges)
