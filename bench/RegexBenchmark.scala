package halotukozak.regex

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * Construction-side benchmarks: smart-constructor normalization (`alt`/`concat`/`repeat`,
 * which drive `Set`-based ACI dedup and therefore `Regex#hashCode`) and parsing.
 *
 * Run: `scala-cli run --jmh . -- RegexBenchmark` (see `bench/README.md`).
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class RegexBenchmark:

  private val pattern = """(foo|bar|baz|qux){2,10}[a-zA-Z0-9_]+\d{3,5}(\.[a-z]{2,4})?"""

  /** Builds a 50-way alternation of distinct literals, one `|` at a time. */
  @Benchmark
  def buildAlternation(): Regex = (0 until 50).foldLeft(Regex.Empty: Regex)((acc, i) => acc | Regex.literal(s"token$i"))

  /** Unfolds a bounded quantifier into its `Concat`/`Alt` expansion. */
  @Benchmark
  def repeatBounded(): Regex = Regex.lit('a').repeat(0, 500)

  @Benchmark
  def parse(): Regex = RegexParser.parse(pattern).toOption.get
