package halotukozak.regex

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/**
 * Construction-side benchmarks: smart-constructor normalization (`alt`/`inter`/`concat`/
 * `repeat`, which drive `Set`-based ACI dedup and therefore `Regex#hashCode`) and parsing.
 * `branches` scales the alternation/intersection benchmarks across input sizes.
 *
 * Run: `scala-cli run --jmh . -- RegexBenchmark` (see `bench/README.md`).
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class RegexBenchmark:

  @Param(Array("10", "50", "200"))
  var branches: Int = uninitialized

  private val pattern = """(foo|bar|baz|qux){2,10}[a-zA-Z0-9_]+\d{3,5}(\.[a-z]{2,4})?"""

  /** Builds a `branches`-way alternation of distinct literals, one `|` at a time. */
  @Benchmark
  def buildAlternation(): Regex = (0 until branches).foldLeft(Regex.Empty: Regex)((acc, i) =>
    acc | Regex.literal(s"token$i"),
  )

  /** Builds a `branches`-way intersection of char-range regexes (the `Chars`-merge path in `inter`). */
  @Benchmark
  def buildIntersection(): Regex = (0 until branches).foldLeft(Regex(CharSet.all): Regex)((acc, i) =>
    acc & Regex.range('a', ('a' + i % 26).toChar),
  )

  /** Unfolds a bounded quantifier into its `Concat`/`Alt` expansion. */
  @Benchmark
  def repeatBounded(): Regex = Regex.lit('a').repeat(0, 500)

  @Benchmark
  def parse(): Regex = RegexParser.parse(pattern).toOption.get

  // Same shape as `pattern` above, wrapped in `(?i)` - the only one of lookahead/anchors/(?i)
  // with real parse-time cost, since case folding expands every literal/range CharSet as it's
  // built (see RegexParser.foldRange/foldCharSet). Directly comparable to `parse` above.
  private val caseInsensitivePattern = s"(?i)$pattern"

  @Benchmark
  def parseCaseInsensitive(): Regex = RegexParser.parse(caseInsensitivePattern).toOption.get
