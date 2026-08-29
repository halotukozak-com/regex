package halotukozak.regex

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/**
 * Benchmarks for [[CaptureMatcher]]'s Pike's-VM NFA engine — a new, isolated hot path (see its
 * own doc comment), not a regression check against [[Subset]]/[[TokenMatcher]] since it doesn't
 * touch either. Just a baseline for this feature: `compile` cost, straightforward `matchWhole`
 * cost, and a many-alternations/many-groups case that empirically confirms the priority-ordered
 * thread simulation's per-PC dedup keeps the live thread count bounded rather than blowing up.
 *
 * Run: `scala-cli run --jmh . -- CaptureBenchmark` (see `bench/README.md`).
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class CaptureBenchmark:

  private val emailPattern = """([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+)\.([a-zA-Z]{2,})"""
  private val emailInput = "user@example.com"

  private def matcherOf(pattern: String): CaptureMatcher = CaptureMatcher.parse(pattern) match
    case Right(m) => m
    case Left(err) => throw IllegalStateException(s"benchmark pattern failed to parse: $err")

  private val emailMatcher = matcherOf(emailPattern)

  @Benchmark
  def compileEmailPattern(): CaptureMatcher = matcherOf(emailPattern)

  @Benchmark
  def matchWholeSimple(): Option[MatchResult] = emailMatcher.matchWhole(emailInput)

  // A 50-way alternation of distinct fixed-length tokens, each capturing, followed by a shared
  // suffix - every branch is a live thread until the first character disambiguates them (they
  // all start with a distinct literal, so they die immediately in practice, but this still
  // stresses the epsilon-closure/Split-chain machinery's per-step bookkeeping at a size where an
  // accidental O(n^2) (e.g. from rebuilding the visited array or thread vector inefficiently)
  // would show up). Meant to be directly comparable to SubsetBenchmark's manyStatesEmpty, which
  // stresses the analogous BFS in Subset's derivative engine.
  private val manyGroupsPattern = (0 until 50).map(i => f"(tok$i%03d)").mkString("|") + "(x*)"
  private val manyGroupsMatcher = matcherOf(manyGroupsPattern)
  private val manyGroupsInput = "tok049" + ("x" * 20)

  @Benchmark
  def matchWholeManyAlternations(): Option[MatchResult] = manyGroupsMatcher.matchWhole(manyGroupsInput)
