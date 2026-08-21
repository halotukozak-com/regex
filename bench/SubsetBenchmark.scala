package halotukozak.regex

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * Benchmarks for the Brzozowski-derivative machinery in [[Subset]] — the core algorithm
 * this library exists for (`subset`/`isEmpty` drive the exact language-containment check).
 *
 * Run: `scala-cli run --jmh . -- SubsetBenchmark` (see `bench/README.md`).
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class SubsetBenchmark:

  private val narrow = Subset.parse("""[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.(com|org|net|io)""").toOption.get
  private val wide = Subset.parse(""".*@.*\..*""").toOption.get

  @Benchmark
  def subsetCheck(): Boolean = narrow.subset(wide)

  @Benchmark
  def isEmptyCheck(): Boolean = narrow.isEmpty

  @Benchmark
  def deriveChain(): Boolean =
    "user@example.com".foldLeft(narrow)((s, c) => s.derive(c.toInt)).nullable
