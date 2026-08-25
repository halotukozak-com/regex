package halotukozak.regex

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/**
 * Matching-side benchmarks for [[TokenMatcher]] — the array-lookup DFA that `RegexBenchmark`
 * (construction) and `SubsetBenchmark` (derivative exploration) don't cover. `matchAt`/
 * `findFirst` are the actual hot path documented on `TokenMatcher`: no `Regex`/`Subset`
 * allocation or derivation once `compile` has run, just a binary search over `boundaries`
 * plus an `Int` transition-table read per code point. `compile` itself (DFA construction via
 * Brzozowski derivatives over all patterns in parallel) is included separately since it's a
 * one-time cost with very different scaling behavior from the steady-state lookups.
 *
 * Run: `scala-cli run --jmh . -- TokenMatcherBenchmark` (see `bench/README.md`).
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class TokenMatcherBenchmark:

  private def parse(pattern: String): Regex = RegexParser.parse(pattern).toOption.get

  // A small realistic lexer: keyword literals racing against a generic identifier rule, plus
  // numeric and whitespace tokens - the same shape as TokenMatcherTest's "real lexer" cases.
  private val lexerPatterns =
    Seq("if", "else", "while", "return", "[a-zA-Z_][a-zA-Z0-9_]*", "[0-9]+(\\.[0-9]+)?", "[ \\t\\n]+")

  private val lexer: TokenMatcher = TokenMatcher.fromRegexes(lexerPatterns.map(parse)*)

  @Param(Array("10", "100", "1000"))
  var length: Int = uninitialized

  private var identifierInput: String = uninitialized
  private var sourceInput: String = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    // Worst case for matchAt: a single long identifier, so the DFA walks `length` transitions
    // before hitting the dead state (end of input) rather than bailing out after a few chars.
    identifierInput = "_" + Iterator.continually('a' to 'z').flatten.take(length - 1).mkString
    // A source-like string of space-separated keywords/identifiers/numbers, for findFirst to
    // scan across many tokens instead of matching once at position 0.
    val tokens = Seq("if", "x1", "42", "return", "while", "y_2", "3.14", "else")
    sourceInput = Iterator.continually(tokens).flatten.take(length).mkString(" ")

  /** Steady-state lookup cost: one long identifier, no dead transitions until EOF. */
  @Benchmark
  def matchAtIdentifier(): Option[(priority: Int, end: Int)] = lexer.matchAt(identifierInput, 0)

  /** Scans a whole token stream, exercising `findFirst`'s skip-then-match loop. */
  @Benchmark
  def findFirstAcrossSource(): Int =
    @annotation.tailrec
    def loop(from: Int, count: Int): Int =
      lexer.findFirst(sourceInput, from) match
        case Some((_, _, end)) => loop(end, count + 1)
        case None => count
    loop(0, 0)

  /** One-time DFA-construction cost, scaling with the number of racing patterns. */
  @Benchmark
  def compileLexer(): TokenMatcher = TokenMatcher.fromRegexes(lexerPatterns.map(parse)*)
