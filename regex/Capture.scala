package halotukozak.regex

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Result of a successful [[CaptureMatcher.matchWhole]] call: the spans captured by every
 * numbered/named group in the pattern (see [[Regex.Group]]). Group 0 is always present and
 * always spans the whole input, exactly like `java.util.regex.Matcher.group(0)`.
 */
final case class MatchResult private[regex] (private val registers: Array[Int], private val names: Map[String, Int]):

  /**
   * The span `(start, end)` captured by group `index`, or [[None]] if that group never
   * participated in the match - e.g. it sits in an alternative branch that wasn't taken, or
   * inside a `*`/`{0,n}` repetition that ran zero times. Distinct from `Some((p, p))`, which
   * means the group *did* participate but captured zero characters (e.g. `(a?)` matching `""`) -
   * see [[Regex.Group]]'s own doc comment for why this distinction matters.
   */
  def group(index: Int): Option[(start: Int, end: Int)] =
    val s = registers(2 * index)
    val e = registers(2 * index + 1)
    if s < 0 || e < 0 then None else Some((start = s, end = e))

  /** Like [[group]], but by a `(?<name>...)` group's name instead of its number. */
  def group(name: String): Option[(start: Int, end: Int)] = names.get(name).flatMap(group)

/**
 * Whole-string capture-span extraction for patterns containing [[Regex.Group]] nodes - the
 * "Phase B" this library's issue #18 tracks (Phase A, [[Regex.Group]] itself, only recognizes
 * groups structurally; nothing extracted their matched spans until this).
 *
 * Backed by Pike's VM: a Thompson-construction NFA compiled directly from a parsed [[Regex]]
 * tree (unerased - unlike [[Subset.of]], this needs every [[Regex.Group]] node, not none of
 * them), executed as a priority-ordered set of "threads" (one per live NFA position) that all
 * advance one input code point at a time, each carrying its own capture-register array - the
 * technique RE2/Go's `regexp`/Rust's `regex` crate actually ship for general (non-"one-pass")
 * captures. Guaranteed `O(programSize × inputLength)`: at most one thread per program
 * instruction survives each step (an equal-or-lower-priority duplicate reaching an
 * already-live instruction is dropped), so there's no repeat of catastrophic-backtracking
 * blowup - the same complexity discipline [[Subset.isEmpty]]'s visited-state BFS already
 * relies on.
 *
 * This is a genuinely separate engine from [[Subset]]/[[TokenMatcher]], not an extension of
 * either - `Subset.of` erases every `Group` before deriving through a pattern specifically so
 * its derivative algebra never has to reason about captures at all. `Regex.scala`'s only
 * changes for this feature are mechanical: [[Regex.Alt]]'s `parts` became an order-preserving
 * `Vector` instead of a `Set` (see its own doc comment for why - every no-backtracking submatch
 * algorithm, Pike's VM included, needs leftmost-first alternation priority, which a `Set`
 * can't record) with `equals`/`hashCode` overridden back to Set-like semantics so `Subset`'s
 * ACI-normalization/termination story is completely unaffected. `Subset.scala`/`TokenMatcher.scala`
 * are untouched.
 *
 * Whole-string match only (mirrors this library's only existing matching precedent,
 * `RegexConformanceTest`'s private `matches` helper) - no `find`/`replace`/`split`, and captures
 * aren't wired into [[TokenMatcher]]'s compiled DFA. Both are tracked separately (issue #53 for
 * the former; a `TokenMatcher` follow-up for the latter).
 */
final class CaptureMatcher private (
  private val program: Array[CaptureMatcher.Inst],
  private val entry: Int,
  private val acceptPc: Int,
  private val numRegisters: Int,
  private val names: Map[String, Int],
):
  import CaptureMatcher.*

  private def numGroups: Int = numRegisters / 2 - 1

  /** Matches `input` from start to end. [[None]] if the pattern doesn't accept the whole string. */
  def matchWhole(input: CharSequence): Option[MatchResult] =
    runToEnd(program, entry, acceptPc, numRegisters, input).map(MatchResult(_, names))

  /**
   * Lets a `CaptureMatcher` be used as a pattern-match extractor - `case myMatcher(g1, g2) =>
   * ...` - mirroring `scala.util.matching.Regex`'s own `unapplySeq`, except a group that never
   * participated in the match comes back as [[None]] rather than `scala.util.matching.Regex`'s
   * `null`-in-a-`List` (see [[MatchResult.group]] for why that distinction exists - the same
   * reasoning applies here). Positional only, one element per numbered group `1..N` - group 0
   * (the whole match) is never included, the same convention `scala.util.matching.Regex` uses -
   * `MatchResult.group(name)` (via [[matchWhole]]) still covers named-group access, which
   * `unapplySeq`'s purely positional binding can't express.
   */
  def unapplySeq(input: CharSequence): Option[Seq[Option[String]]] =
    matchWhole(input).map: result =>
      (1 to numGroups).map(i => result.group(i).map(span => input.subSequence(span.start, span.end).toString))

object CaptureMatcher:

  /** Parses `pattern` and compiles it into a [[CaptureMatcher]] ready for repeated matching. */
  def parse(pattern: String): Either[RegexParseError, CaptureMatcher] =
    RegexParser.parse(pattern).map(compile)

  private def compile(r: Regex): CaptureMatcher =
    val names = collectNames(r)
    val compiler = new NfaCompiler
    val acceptPc = compiler.emit(Inst.Match)
    val closeWhole = compiler.emit(Inst.Save(1, acceptPc))
    val bodyEntry = compiler.compile(r, closeWhole)
    val entry = compiler.emit(Inst.Save(0, bodyEntry))
    new CaptureMatcher(compiler.toProgram, entry, acceptPc, 2 * (numGroups(r) + 1), names)

  private def numGroups(r: Regex): Int =
    @tailrec def loop(pending: List[Regex], best: Int): Int = pending match
      case Nil => best
      case Regex.Group(index, _, inner) :: rest => loop(inner :: rest, math.max(best, index))
      case Regex.Concat(a, b) :: rest => loop(a :: b :: rest, best)
      case Regex.Alt(parts) :: rest => loop(parts.toList ::: rest, best)
      case Regex.Inter(parts) :: rest => loop(parts.toList ::: rest, best)
      case Regex.Star(inner) :: rest => loop(inner :: rest, best)
      case Regex.Repeat(inner, _, _) :: rest => loop(inner :: rest, best)
      case Regex.Compl(inner) :: rest => loop(inner :: rest, best)
      case Regex.Look(inner, _) :: rest => loop(inner :: rest, best)
      case (Regex.Eps | Regex.Empty | Regex.Chars(_) | Regex.StartAnchor) :: rest => loop(rest, best)
    loop(List(r), 0)

  /** Like [[numGroups]]: a plain `@tailrec` worklist loop, for the same stack-safety reason. */
  private def collectNames(r: Regex): Map[String, Int] =
    @tailrec def loop(pending: List[Regex], acc: Map[String, Int]): Map[String, Int] = pending match
      case Nil => acc
      case Regex.Group(index, Some(name), inner) :: rest => loop(inner :: rest, acc + (name -> index))
      case Regex.Group(_, None, inner) :: rest => loop(inner :: rest, acc)
      case Regex.Concat(a, b) :: rest => loop(a :: b :: rest, acc)
      case Regex.Alt(parts) :: rest => loop(parts.toList ::: rest, acc)
      case Regex.Inter(parts) :: rest => loop(parts.toList ::: rest, acc)
      case Regex.Star(inner) :: rest => loop(inner :: rest, acc)
      case Regex.Repeat(inner, _, _) :: rest => loop(inner :: rest, acc)
      case Regex.Compl(inner) :: rest => loop(inner :: rest, acc)
      case Regex.Look(inner, _) :: rest => loop(inner :: rest, acc)
      case (Regex.Eps | Regex.Empty | Regex.Chars(_) | Regex.StartAnchor) :: rest => loop(rest, acc)
    loop(List(r), Map.empty)

  // ---------------------------------------------------------------------------------------
  // NFA compilation - Pike's VM bytecode (Cox, "Regular Expression Matching: the Virtual
  // Machine Approach"), extended with Save for captures. Compiled continuation-passing style:
  // `compile(node, next)` emits node's own instructions ending in a fall-through to the
  // already-known `next` PC, returning node's entry PC - so composition (Concat, Group, ...)
  // just threads PCs, no backpatching, EXCEPT Star/Repeat's self-referential loop-back Split,
  // which reserves its slot before compiling the loop body and patches it once the body's
  // entry PC is known.
  // ---------------------------------------------------------------------------------------

  private enum Inst:
    /** Consume one code point in `set`, advance to `next`. Dies (thread removed) otherwise. */
    case Char(set: CharSet, next: Int)

    /**
     * Zero-width: `^`/`\A`. `$`/`\Z`/`\z` need no equivalent - they're parsed as `(?!.)`
     * ([[RegexParser]]'s `endOfInput`), handled by [[Look]] below like any other lookahead.
     */
    case StartAssert(next: Int)

    /** Zero-width: record the current input position into register `slot`. */
    case Save(slot: Int, next: Int)

    /** Zero-width fork: `x` is tried first (higher priority) - see [[epsilonClosure]]. */
    case Split(x: Int, next: Int)

    /**
     * Zero-width lookahead assertion. `entry`/`acceptPc` compile the lookahead body as its own
     * nested sub-program (sharing the outer program's instruction array) - not delegated to
     * [[Subset]], since `Subset` has no notion of captures at all and a group nested inside a
     * lookahead (`(?=(?<x>a))a`) must still capture correctly.
     */
    case Look(entry: Int, acceptPc: Int, positive: Boolean, next: Int)

    /** Terminal: reached this instruction's own PC. See [[epsilonClosure]] for what that means. */
    case Match

  private final class NfaCompiler:
    private val buf = mutable.ArrayBuffer.empty[Inst]

    def toProgram: Array[Inst] = buf.toArray

    def emit(inst: Inst): Int =
      buf += inst
      buf.length - 1

    private def reserve(): Int = emit(Inst.StartAssert(-1))

    private def patch(pc: Int, inst: Inst): Unit = buf(pc) = inst

    def compile(node: Regex, next: Int): Int = node match
      case Regex.Eps => next
      case Regex.Empty => emit(Inst.Char(CharSet.empty, next)) // matches nothing: any char dies here
      case Regex.Chars(set) => emit(Inst.Char(set, next))
      case Regex.StartAnchor => emit(Inst.StartAssert(next))
      case Regex.Concat(a, b) => compile(a, compile(b, next))
      case Regex.Alt(parts) => compileAlt(parts.toList, next)
      case Regex.Star(inner) => compileStar(inner, next)
      case Regex.Repeat(inner, lo, hi) => compileRepeat(inner, lo, hi, next)
      case Regex.Group(index, _, inner) =>
        val closePc = emit(Inst.Save(2 * index + 1, next))
        emit(Inst.Save(2 * index, compile(inner, closePc)))
      case Regex.Look(inner, positive) =>
        val acceptPc = emit(Inst.Match)
        val innerEntry = compile(inner, acceptPc)
        emit(Inst.Look(innerEntry, acceptPc, positive, next))
      case Regex.Compl(_) | Regex.Inter(_) =>
        // Never produced by RegexParser.parse (`&&`/`[^...]` stay at the CharSet level - see
        // RegexParser.scala; Inter/Compl are only ever built programmatically, e.g. by Subset's
        // own `&`/`!`), so CaptureMatcher.parse's input can't contain either.
        throw MatchError(s"unreachable: CaptureMatcher doesn't support $node (never produced by RegexParser.parse)")

    /**
     * A plain `@tailrec` loop, not `deepRecursive`: recursion depth here tracks branch *count*,
     * not tree depth (an `Alt` isn't capped the way `Regex.repeat` caps `Repeat` bounds, so a
     * pattern spelling out enough `|`-separated branches needs the same stack safety `compile`'s
     * tree recursion does) - but unlike that one, this shape flattens for free: walking
     * `branches.reverse` and folding `Split`s from the lowest-priority branch backward builds
     * the exact same nested-`Split` structure the natural `head :: compileAlt(tail, next)`
     * recursion would (`Split(b1, Split(b2, ..., Split(bn-1, bn)))`), just accumulated instead
     * of nested - so there's no `deepRecursive` dispatch cost to pay per branch either.
     */
    private def compileAlt(branches: List[Regex], next: Int): Int =
      @tailrec def loop(remaining: List[Regex], acc: Int): Int = remaining match
        case Nil => acc
        case head :: rest => loop(rest, emit(Inst.Split(compile(head, next), acc)))
      branches.reverse match
        case Nil => throw MatchError("unreachable: Alt always has >= 2 branches")
        case last :: rest => loop(rest, compile(last, next))

    private def compileStar(inner: Regex, next: Int): Int =
      val splitPc = reserve()
      val innerEntry = compile(inner, splitPc)
      patch(splitPc, Inst.Split(innerEntry, next))
      splitPc

    private def compileRepeat(inner: Regex, lo: Int, hi: Int, next: Int): Int =
      if hi == Int.MaxValue then compileMandatory(inner, lo, compileStar(inner, next))
      else compileBounded(inner, lo, hi, next)

    private def compileBounded(inner: Regex, lo: Int, hi: Int, next: Int): Int =
      @tailrec def optionalTail(remaining: Int, tail: Int): Int =
        if remaining <= 0 then tail
        else
          val splitPc = reserve()
          val innerEntry = compile(inner, tail)
          patch(splitPc, Inst.Split(innerEntry, tail))
          optionalTail(remaining - 1, splitPc)
      compileMandatory(inner, lo, optionalTail(hi - lo, next))

    @tailrec
    private def compileMandatory(inner: Regex, remaining: Int, next: Int): Int =
      if remaining <= 0 then next else compileMandatory(inner, remaining - 1, compile(inner, next))

  // ---------------------------------------------------------------------------------------
  // Runtime - the priority-ordered thread simulation itself. `epsilonClosure` is shared by
  // both the outer whole-string driver (`runToEnd`, which only inspects `accepted` once all
  // input is consumed) and lookahead evaluation (`evalLookahead`, which inspects it after
  // every step, per Look's "some prefix" semantics) - see Inst.Look's doc comment for why
  // lookahead needs its own nested simulation instead of delegating to Subset.
  // ---------------------------------------------------------------------------------------

  private final case class ClosureResult(charThreads: Vector[(Int, Array[Int])], accepted: Option[Array[Int]])

  /**
   * Epsilon-closure of `starts` at input position `pos`: follows every zero-width instruction
   * (`Save`/`Split`/`StartAssert`/`Look`) to the `Char`/`Match` instructions reachable from
   * them, in priority order (each `Split`'s `x` fully explored before its `next`), visiting each
   * program counter at most once - a lower-priority thread reaching an already-visited PC is
   * simply dropped, since (Cox) "the saved pointers do not influence future execution: they only
   * record past execution." That's both what bounds one step's work to `O(programSize)` and what
   * implements leftmost-first disambiguation.
   */
  private def epsilonClosure(
    program: Array[Inst],
    starts: Iterable[(Int, Array[Int])],
    pos: Int,
    input: CharSequence,
    acceptPc: Int,
  ): ClosureResult =
    val visited = new Array[Boolean](program.length)
    val charThreads = mutable.ArrayBuffer.empty[(Int, Array[Int])]
    var accepted: Option[Array[Int]] = None

    def go(pc: Int, registers: Array[Int]): Unit =
      if !visited(pc) then
        visited(pc) = true
        program(pc) match
          case _: Inst.Char => charThreads += ((pc, registers))
          case Inst.Match => if pc == acceptPc && accepted.isEmpty then accepted = Some(registers)
          case Inst.Save(slot, next) =>
            val updated = registers.clone()
            updated(slot) = pos
            go(next, updated)
          case Inst.Split(x, next) =>
            go(x, registers)
            go(next, registers)
          case Inst.StartAssert(next) => if pos == 0 then go(next, registers)
          case Inst.Look(entry, lookAcceptPc, positive, next) =>
            evalLookahead(program, entry, lookAcceptPc, input, pos, registers) match
              case Some(withCaptures) if positive => go(next, withCaptures)
              case None if !positive => go(next, registers)
              case _ => ()

    starts.foreach(go)
    ClosureResult(charThreads.toVector, accepted)

  private def stepChar(program: Array[Inst], charThreads: Vector[(Int, Array[Int])], c: Int)
    : Vector[(Int, Array[Int])] =
    charThreads.flatMap: (pc, registers) =>
      program(pc) match
        case Inst.Char(set, next) if set.contains(c) => Some((next, registers))
        case _ => None

  /**
   * `(?=r)`/`(?!r)`: "some prefix of the string remaining here is in `L(r)`" (see [[Regex.Look]]'s
   * doc), i.e. accept as soon as `acceptPc` is reached at *any* position, not just at end of
   * input - unlike [[runToEnd]]'s whole-string semantics. Bounded by the remaining input length,
   * same as the outer match.
   */
  private def evalLookahead(
    program: Array[Inst],
    entry: Int,
    acceptPc: Int,
    input: CharSequence,
    startPos: Int,
    initialRegisters: Array[Int],
  ): Option[Array[Int]] =
    @tailrec def loop(threads: Iterable[(Int, Array[Int])], pos: Int): Option[Array[Int]] =
      val closure = epsilonClosure(program, threads, pos, input, acceptPc)
      closure.accepted match
        case some @ Some(_) => some
        case None =>
          if closure.charThreads.isEmpty || pos >= input.length then None
          else
            val c = Character.codePointAt(input, pos)
            loop(stepChar(program, closure.charThreads, c), pos + Character.charCount(c))
    loop(List((entry, initialRegisters)), startPos)

  /** Whole-string match: only `accepted` at the final position (`pos == input.length`) counts. */
  private def runToEnd(
    program: Array[Inst],
    entry: Int,
    acceptPc: Int,
    numRegisters: Int,
    input: CharSequence,
  ): Option[Array[Int]] =
    @tailrec def loop(threads: Iterable[(Int, Array[Int])], pos: Int): Option[Array[Int]] =
      val closure = epsilonClosure(program, threads, pos, input, acceptPc)
      if pos >= input.length then closure.accepted
      else if closure.charThreads.isEmpty then None
      else
        val c = Character.codePointAt(input, pos)
        loop(stepChar(program, closure.charThreads, c), pos + Character.charCount(c))
    loop(List((entry, Array.fill(numRegisters)(-1))), 0)
