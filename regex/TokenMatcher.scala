package halotukozak.regex

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, ToExpr, Varargs}

/**
 * Lexer-style longest-match tokenizer over a priority-ordered list of patterns.
 *
 * Backed by a DFA obtained via Brzozowski-derivative subset construction over all patterns
 * in parallel (a single automaton, not one-per-pattern): building it walks the derivative
 * state space once (`fromRegexes`/`fromSubsets`), so `matchAt` itself is just array lookups -
 * a binary search to classify the current code point into an alphabet partition, then an
 * `Int` transition-table read - with no `Regex`/`Subset` allocation or derivation on the hot
 * path. Ties broken by lowest priority index (i.e., the first pattern in the input list wins).
 */
opaque type TokenMatcher = TokenMatcher.Dfa

object TokenMatcher:

  /**
   * Flat DFA table produced by subset construction.
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
   */
  /**
   * Plain class, not a case class: `Array[Int]` fields would give a case class reference-based
   * `equals`/`hashCode` and an unreadable `toString` anyway, and nothing here pattern-matches or
   * `.copy`s a `Dfa` - construction is the only thing needed, and Scala 3's creator-application
   * sugar (`Dfa(...)`) already covers that for a plain class.
   *
   * Constructor kept private to this file - the only places that ever build a `Dfa` are
   * `compile` and `fromDfa` below, both in this same scope. External code (including macro-
   * spliced trees) goes through `fromDfa`, a plain `def` declared to return `TokenMatcher`,
   * not through this constructor directly - see the note on `ToExpr[TokenMatcher]`.
   */
  final class Dfa private[TokenMatcher] (val boundaries: Array[Int], val transitions: Array[Int], val accept: Array[Int]):
    def numPartitions: Int = boundaries.length

  /** Build a matcher from pre-parsed regexes (use this from macros after compile-time parsing). */
  def fromRegexes(initial: Regex*): TokenMatcher = fromSubsets(initial.map(Subset.of)*)

  /** Build a matcher from pre-parsed subsets (use this from macros after compile-time parsing). */
  def fromSubsets(initial: Subset*): TokenMatcher = compile(initial.toIndexedSeq)

  /**
   * Rehydrates a `TokenMatcher` from its flat DFA arrays. Declared to return `TokenMatcher`
   * (not `Dfa`) so that a tree calling it - e.g. the one `ToExpr[TokenMatcher]` builds below,
   * once spliced into a foreign compilation unit - is externally `TokenMatcher`-typed too: a
   * spliced tree's static type is whatever its outermost call is *declared* to return, and
   * opaque-type transparency (`Dfa` being `TokenMatcher` in this object's own scope) doesn't
   * carry over to wherever the tree lands after splicing.
   */
  def fromDfa(boundaries: Array[Int], transitions: Array[Int], accept: Array[Int]): TokenMatcher =
    Dfa(boundaries, transitions, accept)

  private def compile(patterns: IndexedSeq[Subset]): Dfa =
    val boundaries = alphabetPartition(patterns)
    val numPartitions = boundaries.length

    val ids = mutable.LinkedHashMap(patterns -> 0)
    val queue = mutable.Queue(patterns)
    val transitions = mutable.ArrayBuffer.empty[Int]
    val accept = mutable.ArrayBuffer.empty[Int]

    while queue.nonEmpty do
      val state = queue.dequeue()
      accept += firstNullable(state)
      var i = 0
      while i < numPartitions do
        val next = state.map(_.derive(boundaries(i)))
        transitions +=
          (if isDead(next) then -1
           else ids.getOrElseUpdate(next, { val id = ids.size; queue.enqueue(next); id }))
        i += 1

    Dfa(boundaries, transitions.toArray, accept.toArray)

  /**
   * Alphabet partition induced by every pattern's `Chars` leaves, shared across the whole DFA -
   * see the class-level note on [[Dfa.boundaries]] for why one partition suffices for every
   * derivative state, not just the initial ones.
   */
  private def alphabetPartition(patterns: IndexedSeq[Subset]): Array[Int] =
    (SortedSet(0, CharSet.maxCodePoint + 1) ++ patterns.iterator.flatMap(_.underlying.alphabetBoundaries)).init.toArray

  private def firstNullable(state: IndexedSeq[Subset]): Int =
    state.iterator.zipWithIndex.collectFirst { case (sub, idx) if sub.nullable => idx }.getOrElse(-1)

  private def isDead(state: IndexedSeq[Subset]): Boolean = state.forall(_ == Subset.empty)

  /** Largest `i` with `boundaries(i) <= c`; well-defined since `boundaries(0) == 0 <= c` always. */
  private def partitionIndex(boundaries: Array[Int], c: Int): Int =
    @tailrec
    def loop(lo: Int, hi: Int): Int =
      if lo == hi then lo
      else
        val mid = (lo + hi + 1) >>> 1
        if boundaries(mid) <= c then loop(mid, hi) else loop(lo, mid - 1)
    loop(0, boundaries.length - 1)

  extension (m: TokenMatcher)

    /**
     * Match a token starting at `start` in `input`.
     *
     * @return `Some((priority, end))` where `priority` is the index of the winning
     *         pattern and `end` is the exclusive end of the longest match. `None` if
     *         no pattern matches any (possibly empty) prefix at `start`.
     */
    def matchAt(input: CharSequence, start: Int): Option[(priority: Int, end: Int)] =
      val dfa = m
      @tailrec
      def loop(state: Int, pos: Int, bestPriority: Int, bestEnd: Int): Option[(priority: Int, end: Int)] =
        if pos >= input.length then endResult(bestPriority, bestEnd)
        else
          val c = Character.codePointAt(input, pos)
          val next = dfa.transitions(state * dfa.numPartitions + partitionIndex(dfa.boundaries, c))
          if next < 0 then endResult(bestPriority, bestEnd)
          else
            val nextPos = pos + Character.charCount(c)
            dfa.accept(next) match
              case acc if acc >= 0 => loop(next, nextPos, acc, nextPos)
              case _ => loop(next, nextPos, bestPriority, bestEnd)
      def endResult(priority: Int, end: Int) = if end >= 0 then Some((priority = priority, end = end)) else None
      val initialAccept = dfa.accept(0)
      loop(0, start, initialAccept, if initialAccept >= 0 then start else -1)

    /** First position `>= from` at which some pattern matches a non-empty prefix. */
    def findFirst(input: CharSequence, from: Int): Option[(start: Int, priority: Int, end: Int)] =
      codePointStarts(input, from)
        .map(start => (start, m.matchAt(input, start)))
        .collectFirst:
          case (start, Some((priority, end))) if end > start => (start, priority, end)

  /** Iterator of code-point start offsets in `input`, beginning at `from`. */
  private def codePointStarts(input: CharSequence, from: Int): Iterator[Int] =
    Iterator
      .iterate(from)(p => p + Character.charCount(Character.codePointAt(input, p)))
      .takeWhile(_ < input.length)

  // $COVERAGE-OFF$
  /** Embeds the already-built DFA table as `Array[Int]` literals - no `Regex`/`Subset` involved. */
  given ToExpr[TokenMatcher]:
    def apply(m: TokenMatcher)(using Quotes): Expr[TokenMatcher] =
      val dfa = m
      '{
        TokenMatcher.fromDfa(
          ${ intArrayExpr(dfa.boundaries) },
          ${ intArrayExpr(dfa.transitions) },
          ${ intArrayExpr(dfa.accept) },
        )
      }

  private def intArrayExpr(arr: Array[Int])(using Quotes): Expr[Array[Int]] =
    '{ Array[Int](${ Varargs(arr.toSeq.map(Expr(_))) }*) }
  // $COVERAGE-ON$
