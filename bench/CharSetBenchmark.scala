package halotukozak.regex

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * `CharSet.contains` currently does a linear scan over sorted, non-overlapping ranges —
 * a candidate for a binary search. This benchmark isolates that operation with many
 * disjoint ranges so the two strategies are actually distinguishable.
 *
 * Run: `scala-cli run --jmh . -- CharSetBenchmark` (see `bench/README.md`).
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class CharSetBenchmark:

  private val manyRanges: CharSet = CharSet.normalize((0 until 2000).map(i => Range(i * 4, i * 4 + 1)))

  /** Worst case for a linear scan: the match is the very last range. */
  @Benchmark
  def containsNearEnd(): Boolean = manyRanges.contains(manyRanges.ranges.last.lo)

  /** Best case for a linear scan: the match is the very first range. */
  @Benchmark
  def containsNearStart(): Boolean = manyRanges.contains(0)

  /** A miss (falls in a gap), which still has to prove absence across the whole set. */
  @Benchmark
  def containsMiss(): Boolean = manyRanges.contains(manyRanges.ranges.last.lo + 2)
