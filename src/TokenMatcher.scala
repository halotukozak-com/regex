package halotukozak.regex

import scala.annotation.{nowarn, publicInBinary, tailrec}
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

@publicInBinary private[regex] def tokenMatcherImpl(patternsExpr: Expr[Seq[String]])(using quotes: Quotes)
  : Expr[TokenMatcher] =
  import quotes.reflect.*
  patternsExpr match
    case Varargs(patternExprs) =>
      val regexes = patternExprs.map { patternExpr =>
        patternExpr.value match
          case None =>
            report.errorAndAbort("tokenMatcher patterns must be string literals known at compile time", patternExpr)
          case Some(pattern) =>
            RegexParser.parse(pattern) match
              case Left(error) => report.errorAndAbort(s"Regex parse error: ${error.toString}", patternExpr)
              case Right(regex) => regex
      }
      TokenMatcher.fromRegexesChecked(regexes*) match
        case Left(error) =>
          val patternExpr = patternExprs(error.patternIndex)
          report.errorAndAbort(s"""${error.message} (in "${patternExpr.valueOrAbort}")""", patternExpr)
        case Right(matcher) => Expr(matcher)
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
 * `accept(state)` is the lowest pattern index nullable in that state at a position followed by
 * more input, or `-1` if none is; `acceptAtEnd(state)` is the same at the end of the input. They
 * differ only for `$`/`\Z`/`\z` (see [[Regex.nullableBeforeMoreInput]]): a token ends wherever the
 * next one starts, so those hold only when it's the last one. Any other lookahead would have to
 * see the input after the token, so the builders reject it - see [[UnsupportedTokenConstruct]].
 *
 * State 0 starts a match past the start of the input, `inputStartState` one at offset 0. They
 * differ only when a pattern uses `^`/`\A`, which holds only at offset 0 (state 0 starts from the
 * patterns with every `^`/`\A` already failed, like any derivative). `fastAscii` is built from
 * state 0, so it's skipped for a match at offset 0 whenever the two differ.
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
  private val acceptAtEnd: Array[Int],
  private val fastAscii: Array[Int],
  private val inputStartState: Int,
):

  /** The shape `tokenMatcher(...)` expansions compiled against 0.2.x still call; nothing here does. */
  @nowarn("msg=unused private member")
  @publicInBinary private def this(
    boundaries: Array[Int],
    transitions: Array[Int],
    accept: Array[Int],
    fastAscii: Array[Int],
  ) = this(boundaries, transitions, accept, accept, fastAscii, 0)

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
      if start < input.length && (start > 0 || inputStartState == 0) then
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
          acceptAt(next, nextPos) match
            case acc if acc >= 0 => loop(next, nextPos, acc, nextPos)
            case _ => loop(next, nextPos, bestPriority, bestEnd)
    def acceptAt(state: Int, pos: Int) = if pos < input.length then accept(state) else acceptAtEnd(state)
    def endResult(priority: Int, end: Int) = if end >= 0 then (priority = priority, end = end) else null
    val initialState = if start == 0 then inputStartState else 0
    val initialAccept = acceptAt(initialState, start)
    loop(initialState, start, initialAccept, if initialAccept >= 0 then start else -1)

  /** First position `>= from` at which some pattern matches a non-empty prefix. */
  def findFirst(input: CharSequence, from: Int): Option[(start: Int, priority: Int, end: Int)] =
    TokenMatcher
      .codePointStarts(input, from)
      .map(start => (start, matchAt(input, start)))
      .collectFirst:
        case (start, m) if m != null && m.end > start => (start, m.priority, m.end)

/**
 * Pattern `patternIndex` of a [[TokenMatcher]] uses `construct`, which depends on the input
 * after the token and so can't be matched by it: a lookahead other than the `$`/`\Z`/`\z`
 * anchors. Returned by [[TokenMatcher.fromRegexesChecked]]; the other builders throw it as an
 * `IllegalArgumentException`. Whole-string matching ([[Subset]], [[CaptureMatcher]]) supports
 * every lookahead.
 */
final case class UnsupportedTokenConstruct(patternIndex: Int, construct: String):
  def message: String = s"$construct is not supported in a TokenMatcher pattern"
  override def toString: String = s"$message (pattern $patternIndex)"

object TokenMatcher:

  /**
   * Build a matcher from pre-parsed regexes (use this from macros after compile-time parsing).
   *
   * @throws IllegalArgumentException if a pattern uses a construct [[fromRegexesChecked]] rejects
   */
  def fromRegexes(initial: Regex*): TokenMatcher = fromSubsets(initial.map(Subset.of)*)

  /**
   * Build a matcher from pre-parsed subsets (use this from macros after compile-time parsing).
   *
   * @throws IllegalArgumentException if a pattern uses a construct [[fromRegexesChecked]] rejects
   */
  def fromSubsets(initial: Subset*): TokenMatcher =
    requireSupported(initial)
    compile(initial, Int.MaxValue).getOrElse(throw MatchError("unreachable: Int.MaxValue state cap exceeded"))

  /**
   * Like [[fromRegexes]], but returns the first [[UnsupportedTokenConstruct]] as a `Left` instead
   * of throwing it.
   */
  def fromRegexesChecked(initial: Regex*): Either[UnsupportedTokenConstruct, TokenMatcher] =
    val patterns = initial.map(Subset.of)
    unsupportedIn(patterns)
      .nextOption()
      .toLeft(compile(patterns, Int.MaxValue).getOrElse(throw MatchError("unreachable: Int.MaxValue state cap exceeded")))

  /**
   * Every pattern that uses a construct [[fromRegexesChecked]] rejects, with the first such
   * construct in it, in pattern order - the check those builders make, without building the
   * matcher. Empty when every pattern is supported. E.g. for a macro that reports each
   * unsupported pattern at compile time and builds the matcher at runtime.
   */
  def unsupportedConstructs(patterns: Regex*): List[UnsupportedTokenConstruct] =
    unsupportedIn(patterns.map(Subset.of)).toList

  /**
   * Like [[fromRegexes]], but fails fast with `Left(StateSpaceLimitExceeded(maxStates))` instead
   * of exploring more than `maxStates` distinct derivative states while building the DFA - see
   * [[StateSpaceLimitExceeded]]. Opt-in: [[fromRegexes]] itself stays uncapped for existing
   * callers.
   *
   * @throws IllegalArgumentException if a pattern uses a construct [[fromRegexesChecked]] rejects
   */
  def fromRegexesBounded(maxStates: Int)(initial: Regex*): Either[StateSpaceLimitExceeded, TokenMatcher] =
    fromSubsetsBounded(maxStates)(initial.map(Subset.of)*)

  /** Like [[fromSubsets]], but capped the way [[fromRegexesBounded]] caps [[fromRegexes]]. */
  def fromSubsetsBounded(maxStates: Int)(initial: Subset*): Either[StateSpaceLimitExceeded, TokenMatcher] =
    requireSupported(initial)
    compile(initial, maxStates)

  private def requireSupported(patterns: Seq[Subset]): Unit =
    unsupportedIn(patterns).nextOption().foreach(error => throw IllegalArgumentException(error.toString))

  /** Each pattern that uses a construct a token can't be matched with - see [[UnsupportedTokenConstruct]]. */
  private def unsupportedIn(patterns: Seq[Subset]): Iterator[UnsupportedTokenConstruct] =
    @tailrec
    def constructIn(pending: List[Regex]): Option[String] = pending match
      case Nil => None
      case RegexParser.endOfInput :: rest => constructIn(rest)
      case Regex.Look(_, positive) :: _ =>
        Some(if positive then "lookahead `(?=...)`" else "negative lookahead `(?!...)`")
      case Regex.Concat(first, second) :: rest => constructIn(first :: second :: rest)
      case Regex.Alt(parts) :: rest => constructIn(parts.toList ++ rest)
      case Regex.Inter(parts) :: rest => constructIn(parts.toList ++ rest)
      case Regex.Star(inner) :: rest => constructIn(inner :: rest)
      case Regex.Repeat(inner, _, _) :: rest => constructIn(inner :: rest)
      case Regex.Compl(inner) :: rest => constructIn(inner :: rest)
      case Regex.Group(_, _, inner) :: rest => constructIn(inner :: rest)
      case (Regex.Eps | Regex.Empty | Regex.Chars(_) | Regex.StartAnchor) :: rest => constructIn(rest)
    patterns.iterator.zipWithIndex.flatMap: (pattern, patternIndex) =>
      constructIn(List(pattern.underlying)).map(UnsupportedTokenConstruct(patternIndex, _))

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
        acceptAtEnd: Vector[Int],
      ): (transitions: Array[Int], accept: Array[Int], acceptAtEnd: Array[Int]) =
        queue.dequeueOption match
          case None => (transitions = transitions.toArray, accept = accept.toArray, acceptAtEnd = acceptAtEnd.toArray)
          case Some((state, rest)) =>
            val (row, newIds, discovered) = deriveRow(state, ids)
            loop(
              rest.enqueueAll(discovered),
              newIds,
              transitions ++ row,
              accept :+ firstAccepting(state)(_.underlying.nullableBeforeMoreInput),
              acceptAtEnd :+ firstAccepting(state)(_.nullable),
            )

      // State 0 is the start past offset 0 (no `^`/`\A` holds), then the start at offset 0 if different.
      val initialStates = Seq(patterns.map(_.withoutStartAnchor), patterns).distinct
      if initialStates.sizeIs > maxStates then break(Left(StateSpaceLimitExceeded(maxStates)))
      val (transitions, accept, acceptAtEnd) =
        loop(Queue.from(initialStates), initialStates.zipWithIndex.toMap, Vector.empty, Vector.empty, Vector.empty)
      val numPartitions = boundaries.length
      val fastAscii = buildFastAscii(boundaries, transitions, accept, acceptAtEnd, numPartitions)
      Right(new TokenMatcher(boundaries, transitions, accept, acceptAtEnd, fastAscii, initialStates.size - 1))

  private def firstAccepting(state: Seq[Subset])(accepts: Subset => Boolean): Int =
    state.iterator.zipWithIndex.collectFirst { case (sub, idx) if accepts(sub) => idx }.getOrElse(-1)

  /**
   * Precomputes, for every ASCII code point, whether matching it as a single character from the
   * initial state (0) is unambiguously the *longest* possible match -- i.e. no live pattern could
   * extend that one-character match further. That's true exactly when: (a) consuming the
   * character from state 0 lands on a live state, (b) that state is itself accepting (some
   * pattern's language contains just that one character), the same way whether or not the input
   * ends after it (so not for `a$`), and (c) that state has no live
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
  private def buildFastAscii(
    boundaries: Array[Int],
    transitions: Array[Int],
    accept: Array[Int],
    acceptAtEnd: Array[Int],
    numPartitions: Int,
  ): Array[Int] =
    Array.tabulate(128): c =>
      val next = transitions(partitionIndex(boundaries, c))
      if next < 0 then -1
      else
        val priority = accept(next)
        if priority < 0 || priority != acceptAtEnd(next) then -1
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
          ${ Expr(m.acceptAtEnd) },
          ${ Expr(m.fastAscii) },
          ${ Expr(m.inputStartState) },
        )
      }
  // $COVERAGE-ON$
