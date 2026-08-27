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
  def matchAtIdentifier(): (priority: Int, end: Int) | Null = lexer.matchAt(identifierInput, 0)

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

  // A punctuation-only racing set: every pattern is a single ASCII literal with no live
  // continuation after it, so every match here should take `matchAt`'s `fastAscii` bypass
  // (see that field's doc comment) instead of the general derivative-DFA walk.
  private val punctPatterns = Seq("\\{", "\\}", ",", ":", "\\(", "\\)", ";", "\\+", "-", "\\*", "/")
  private val punctMatcher: TokenMatcher = TokenMatcher.fromRegexes(punctPatterns.map(parse)*)
  private val punctChars: Array[Char] = Array('{', '}', ',', ':', '(', ')', ';', '+', '-', '*', '/')
  private var punctInput: String = uninitialized

  @Setup(Level.Trial)
  def setupPunct(): Unit =
    val rnd = new scala.util.Random(42)
    punctInput = Iterator.continually(punctChars(rnd.nextInt(punctChars.length))).take(length).mkString

  /**
   * Steady-state cost of matching many short (single-char) tokens back to back -- the case
   * `fastAscii` targets, as opposed to `matchAtIdentifier`'s one-long-match case above.
   */
  @Benchmark
  def matchAtPunctuation(): Int =
    var pos = 0
    var count = 0
    while pos < punctInput.length do
      punctMatcher.matchAt(punctInput, pos) match
        case null => pos += 1
        case m => pos = m.end; count += 1
    count

  /**
   * Hand-written charAt dispatch (compiles to a tableswitch) -- the ceiling `fastAscii` chases,
   * included for context on how close the array-lookup bypass gets to a hardcoded dispatch.
   */
  @Benchmark
  def matchAtPunctuationCharAtBaseline(): Int =
    var pos = 0
    var count = 0
    while pos < punctInput.length do
      punctInput.charAt(pos) match
        case '{' | '}' | ',' | ':' | '(' | ')' | ';' | '+' | '-' | '*' | '/' => count += 1
        case _ => ()
      pos += 1
    count
