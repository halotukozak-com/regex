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

  // Shares a long common prefix with `narrow` but drops the `io` alternative, so `subset`
  // has to derive deep before it can tell the two languages apart — a true-negative case,
  // as opposed to `wide` above which is an obvious (and cheap-to-confirm) superset.
  private val almostWide = Subset.parse("""[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.(com|org|net)""").toOption.get

  // A string can't end in both 'b' and 'c', so this intersection's language is empty — but
  // unlike e.g. `[a-z]+ & [A-Z]+`, the smart constructors can't see that at construction
  // time (neither operand is a bare `Chars` node, so the CharSet-merge fast path in `inter`
  // never fires). Proving it empty needs the real thing: isEmptyImpl's BFS has to explore
  // derivatives across the whole `[a-z]*` middle section before it can conclude there's no
  // reachable accepting state.
  private val disjointIntersection = Subset.of(
    Subset.parse("a[a-z]*b").toOption.get.underlying & Subset.parse("a[a-z]*c").toOption.get.underlying,
  )

  @Benchmark
  def subsetCheck(): Boolean = narrow.subset(wide)

  @Benchmark
  def subsetCheckNegative(): Boolean = narrow.subset(almostWide)

  @Benchmark
  def isEmptyCheck(): Boolean = narrow.isEmpty

  @Benchmark
  def isEmptyCheckWorstCase(): Boolean = disjointIntersection.isEmpty

  @Benchmark
  def deriveChain(): Boolean =
    "user@example.com".foldLeft(narrow)((s, c) => s.derive(c.toInt)).nullable
