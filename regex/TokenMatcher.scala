package halotukozak.regex

import scala.annotation.{publicInBinary, tailrec}
import scala.collection.immutable.{Queue, SortedSet}
import scala.quoted.{Expr, Quotes, ToExpr, Varargs}
import scala.util.boundary
import scala.util.boundary.break

/**
 * Compile-time counterpart to `regex"..."` (see [[Regex]]), but for a whole priority-ordered
 * pattern list instead of a single pattern: parses every pattern *and* builds the DFA
 * (`TokenMatcher.compile`'s derivative-state BFS) at macro-expansion time, splicing the
 * already-built matcher through the existing `given ToExpr[TokenMatcher]` as plain `Array[Int]`
 * tables. Without this, `TokenMatcher.fromRegexes`/`fromSubsets` re-derive that whole automaton
 * (BFS over derivative states, transition-table construction) from scratch on every process
 * startup, even though a literal, fixed pattern list (e.g. a lexer's token table) is exactly as
 * knowable at compile time as a single `regex"..."` pattern is.
 *
 * Every pattern must itself reduce to a literal `String` constant - a `Varargs` element that
 * doesn't is a macro-expansion error, not a runtime-parsing fallback: unlike `regex"..."`
 * (which falls back to parsing at runtime when the pattern isn't a literal, since there's
 * exactly one `Regex` either way), there's no equally sensible fallback for a whole vararg
 * pattern list threaded through a macro parameter - `TokenMatcher.fromRegexes`/`fromSubsets`
 * already exist as the explicit runtime-parsing entry point for patterns not known until
 * runtime (e.g. loaded from a config file).
 *
 * `patterns` must be declared `inline` too, for the same reason `regex"..."`'s `sc` receiver
 * is (see that doc comment): without it, the compiler binds the vararg call-site arguments to
 * a synthetic proxy val before splicing, so `'patterns` inside [[tokenMatcherImpl]] sees only a
 * reference to that val - never the literal argument list itself - and every call falls into
 * the "not a literal" branch below regardless of what was actually written at the call site.
 */
inline def tokenMatcher(inline patterns: String*): TokenMatcher = ${ tokenMatcherImpl('patterns) }

private def tokenMatcherImpl(patternsExpr: Expr[Seq[String]])(using quotes: Quotes): Expr[TokenMatcher] =
  import quotes.reflect.*
  patternsExpr match
    case Varargs(patternExprs) =>
      val regexes = patternExprs.map { patternExpr =>
        patternExpr.value match
          case None =>
            report.errorAndAbort("tokenMatcher patterns must be string literals known at compile time", patternExpr)
          case Some(pattern) =>
            RegexParser.parse(pattern) match
              case Left(error) => report.errorAndAbort(s"Regex parse error: $error", patternExpr)
              case Right(regex) => regex
      }
      Expr(TokenMatcher.fromRegexes(regexes*))
    case _ =>
      report.errorAndAbort("tokenMatcher requires a literal, statically-known list of pattern strings", patternsExpr)

/**
 * Lexer-style longest-match tokenizer over a priority-ordered list of patterns.
 *
 * Backed by a DFA obtained via Brzozowski-derivative subset construction over all patterns
 * in parallel (a single automaton, not one-per-pattern): building it walks the derivative
 * state space once (`fromRegexes`/`fromSubsets`), so `matchAt` itself is just array lookups -
 * a binary search to classify the current code point into an alphabet partition, then an
 * `Int` transition-table read - with no `Regex`/`Subset` allocation or derivation on the hot
 * path. Ties broken by lowest priority index (i.e., the first pattern in the input list wins).
 *
 * `boundaries(i)` is the first code point of partition `i` (`boundaries(0) == 0`); the
 * partition itself spans `[boundaries(i), boundaries(i + 1))`, with `boundaries.length` acting
 * as the exclusive upper partition for the last one. All derivative states share this same
 * partition: deriving only ever discards `Chars` leaves (via smart-constructor normalization),
 * never introduces new ones, so the alphabet fixed at the initial patterns already bounds
 * every reachable state.
 *
 * `transitions` is `numStates * boundaries.length` entries, row-major by state; `-1` marks the
 * (unrepresented) dead state, i.e. every pattern's derivative going empty.
 *
 * `accept(state)` is the lowest pattern index nullable in that state, or `-1` if none is.
 *
 * A pattern containing [[Regex.Group]] nodes (capturing/named groups) compiles fine - they're
 * erased the same way [[Subset.of]] erases them for containment checks (`fromRegexes`/
 * `fromSubsets` go through `Subset.of` too) - but `matchAt`/`findFirst` never return capture
 * spans; this DFA only ever tracks priority/end position. Wiring captures into the compiled
 * automaton is separate, not-yet-done work.
 *
 * Constructor kept private - the only place that ever builds one directly is `compile` below,
 * in the companion object; `@publicInBinary` exists solely so `given ToExpr[TokenMatcher]`'s
 * quoted call to it still resolves once that quote is spliced into a different compilation
 * unit's generated bytecode (by `tokenMatcher(...)` below, or a user's own macro), despite
 * being `private` at the source level everywhere else.
 */
final class TokenMatcher @publicInBinary private (
  private val boundaries: Array[Int],
  private val transitions: Array[Int],
  private val accept: Array[Int],
  private val fastAscii: Array[Int],
):
  private def numPartitions: Int = boundaries.length

  /**
   * Match a token starting at `start` in `input`.
   *
   * Returns the bare named tuple (no `Option` wrapper) to avoid allocating a `Some` on every
   * call -- `matchAt` is the innermost hot-path operation, called once per code point during
   * tokenization, and a lexer racing many short/single-character patterns (the common case for
   * punctuation-heavy grammars) spends most of its time in calls that return almost immediately.
   * Test with `== null` (or match against `null`) for absence, same as any other `X | Null`.
   *
   * Checks `fastAscii` first: for an ASCII start character whose one-character match is
   * unambiguously the *longest* possible one (no live pattern can extend it further -- see
   * `fastAscii`'s doc comment), this returns straight from a single array read instead of
   * running the general derivative-DFA walk below (`codePointAt` decode, binary-search
   * partition classification, transition/accept array reads, and -- for single-character
   * patterns specifically -- a second loop iteration just to confirm no continuation exists).
   * Falls through to the general walk for every other case, including non-ASCII starts and
   * ASCII starts where a longer match might still be possible (e.g. `-` when `->` is also a
   * pattern), so this is purely an optimization: identical results either way.
   */
  def matchAt(input: CharSequence, start: Int): (priority: Int, end: Int) | Null =
    val fastPriority =
      if start < input.length then
        val c0 = input.charAt(start)
        if c0 < 128 then fastAscii(c0) else -1
      else -1
    if fastPriority >= 0 then (priority = fastPriority, end = start + 1) else matchAtSlow(input, start)

  private def matchAtSlow(input: CharSequence, start: Int): (priority: Int, end: Int) | Null =
    @tailrec
    def loop(state: Int, pos: Int, bestPriority: Int, bestEnd: Int): (priority: Int, end: Int) | Null =
      if pos >= input.length then endResult(bestPriority, bestEnd)
      else
        val c = Character.codePointAt(input, pos)
        val next = transitions(state * numPartitions + TokenMatcher.partitionIndex(boundaries, c))
        if next < 0 then endResult(bestPriority, bestEnd)
        else
          val nextPos = pos + Character.charCount(c)
          accept(next) match
            case acc if acc >= 0 => loop(next, nextPos, acc, nextPos)
            case _ => loop(next, nextPos, bestPriority, bestEnd)
    def endResult(priority: Int, end: Int) = if end >= 0 then (priority = priority, end = end) else null
    val initialAccept = accept(0)
    loop(0, start, initialAccept, if initialAccept >= 0 then start else -1)

  /** First position `>= from` at which some pattern matches a non-empty prefix. */
  def findFirst(input: CharSequence, from: Int): Option[(start: Int, priority: Int, end: Int)] =
    TokenMatcher
      .codePointStarts(input, from)
      .map(start => (start, matchAt(input, start)))
      .collectFirst:
        case (start, m) if m != null && m.end > start => (start, m.priority, m.end)

object TokenMatcher:

  /** Build a matcher from pre-parsed regexes (use this from macros after compile-time parsing). */
  def fromRegexes(initial: Regex*): TokenMatcher = fromSubsets(initial.map(Subset.of)*)

  /** Build a matcher from pre-parsed subsets (use this from macros after compile-time parsing). */
  def fromSubsets(initial: Subset*): TokenMatcher =
    compile(initial, Int.MaxValue).getOrElse(throw MatchError("unreachable: Int.MaxValue state cap exceeded"))

  /**
   * Like [[fromRegexes]], but fails fast with `Left(StateSpaceLimitExceeded(maxStates))` instead
   * of exploring more than `maxStates` distinct derivative states while building the DFA - see
   * [[StateSpaceLimitExceeded]]. Opt-in: [[fromRegexes]] itself stays uncapped for existing
   * callers.
   */
  def fromRegexesBounded(maxStates: Int)(initial: Regex*): Either[StateSpaceLimitExceeded, TokenMatcher] =
    fromSubsetsBounded(maxStates)(initial.map(Subset.of)*)

  /** Like [[fromSubsets]], but capped the way [[fromRegexesBounded]] caps [[fromRegexes]]. */
  def fromSubsetsBounded(maxStates: Int)(initial: Subset*): Either[StateSpaceLimitExceeded, TokenMatcher] =
    compile(initial, maxStates)

  private def compile(patterns: Seq[Subset], maxStates: Int): Either[StateSpaceLimitExceeded, TokenMatcher] =
    boundary:
      val boundaries = SortedSet(0, CharSet.maxCodePoint + 1)
        .concat(patterns.iterator.flatMap(_.underlying.alphabetBoundaries))
        .init
        .toArray

      def isDead(state: Seq[Subset]): Boolean = state.forall(_ == Subset.empty)

      /**
       * Derives `state` across every alphabet partition, assigning a fresh id (via `ids`) to any
       * not-yet-seen resulting state. `discovered` lists those newly-seen states, in partition
       * order, for the caller to enqueue for later exploration. `break`s out of the enclosing
       * `compile` [[boundary]] the moment a discovery would push the visited-state count past
       * `maxStates` - see [[StateSpaceLimitExceeded]].
       */
      def deriveRow(
        state: Seq[Subset],
        ids: Map[Seq[Subset], Int],
      ): (row: Vector[Int], ids: Map[Seq[Subset], Int], discovered: List[Seq[Subset]]) =
        boundaries.indices.foldLeft((row = Vector.empty[Int], ids = ids, discovered = List.empty[Seq[Subset]])):
          case ((row, ids, discovered), i) =>
            val next = state.map(_.derive(boundaries(i)))
            if isDead(next) then (row = row :+ -1, ids = ids, discovered = discovered)
            else
              ids.get(next) match
                case Some(id) => (row = row :+ id, ids = ids, discovered = discovered)
                case None =>
                  val id = ids.size
                  if id >= maxStates then break(Left(StateSpaceLimitExceeded(maxStates)))
                  (row = row :+ id, ids = ids.updated(next, id), discovered = discovered :+ next)

      @tailrec
      def loop(
        queue: Queue[Seq[Subset]],
        ids: Map[Seq[Subset], Int],
        transitions: Vector[Int],
        accept: Vector[Int],
      ): (transitions: Array[Int], accept: Array[Int]) =
        queue.dequeueOption match
          case None => (transitions = transitions.toArray, accept = accept.toArray)
          case Some((state, rest)) =>
            val (row, newIds, discovered) = deriveRow(state, ids)
            loop(rest.enqueueAll(discovered), newIds, transitions ++ row, accept :+ firstNullable(state))

      if maxStates < 1 then break(Left(StateSpaceLimitExceeded(maxStates)))
      val (transitions, accept) = loop(Queue(patterns), Map(patterns -> 0), Vector.empty, Vector.empty)
      val numPartitions = boundaries.length
      val fastAscii = buildFastAscii(boundaries, transitions, accept, numPartitions)
      Right(new TokenMatcher(boundaries, transitions, accept, fastAscii))

  private def firstNullable(state: Seq[Subset]): Int =
    state.iterator.zipWithIndex.collectFirst { case (sub, idx) if sub.nullable => idx }.getOrElse(-1)

  /**
   * Precomputes, for every ASCII code point, whether matching it as a single character from the
   * initial state (0) is unambiguously the *longest* possible match -- i.e. no live pattern could
   * extend that one-character match further. That's true exactly when: (a) consuming the
   * character from state 0 lands on a live state, (b) that state is itself accepting (some
   * pattern's language contains just that one character), and (c) that state has no live
   * outgoing transitions at all (every continuation is dead, so no longer match is reachable from
   * here). Under those three conditions `matchAt` can return `(priority, start + 1)` straight from
   * this table instead of running the general derivative-DFA walk -- see `matchAt`'s doc comment.
   *
   * Restricted to ASCII (0-127) both because that's the overwhelmingly common case for
   * punctuation/operator tokens in real grammars, and to keep this a flat `Array[Int]` sized by a
   * compile-time constant rather than by the pattern set's alphabet.
   *
   * @return array of length 128; `fastAscii(c)` is the winning pattern's priority, or -1 if `c`
   *         doesn't satisfy the three conditions above (matchAt must fall back to the general walk).
   */
  private def buildFastAscii(boundaries: Array[Int], transitions: Array[Int], accept: Array[Int], numPartitions: Int)
    : Array[Int] =
    Array.tabulate(128): c =>
      val next = transitions(partitionIndex(boundaries, c))
      if next < 0 then -1
      else
        val priority = accept(next)
        if priority < 0 then -1
        else
          val rowStart = next * numPartitions
          val hasLiveContinuation = (0 until numPartitions).exists(p => transitions(rowStart + p) >= 0)
          if hasLiveContinuation then -1 else priority

  /** Largest `i` with `boundaries(i) <= c`; well-defined since `boundaries(0) == 0 <= c` always. */
  private def partitionIndex(boundaries: Array[Int], c: Int): Int =
    @tailrec
    def loop(lo: Int, hi: Int): Int =
      if lo == hi then lo
      else
        val mid = (lo + hi + 1) >>> 1
        if boundaries(mid) <= c then loop(mid, hi) else loop(lo, mid - 1)
    loop(0, boundaries.length - 1)

  /** Iterator of code-point start offsets in `input`, beginning at `from`. */
  private def codePointStarts(input: CharSequence, from: Int): Iterator[Int] =
    Iterator
      .iterate(from)(p => p + Character.charCount(Character.codePointAt(input, p)))
      .takeWhile(_ < input.length)

  // $COVERAGE-OFF$
  /** Embeds the already-built DFA table as `Array[Int]` literals - no `Regex`/`Subset` involved. */
  given ToExpr[TokenMatcher]:
    def apply(m: TokenMatcher)(using Quotes): Expr[TokenMatcher] =
      '{
        TokenMatcher(
          ${ Expr(m.boundaries) },
          ${ Expr(m.transitions) },
          ${ Expr(m.accept) },
          ${ Expr(m.fastAscii) },
        )
      }
  // $COVERAGE-ON$
