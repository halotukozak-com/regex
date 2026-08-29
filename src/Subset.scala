package halotukozak.regex

import halotukozak.commons.deepRecursive
import halotukozak.regex.Regex.{Empty, Eps, *}

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, SortedSet}
import scala.quoted.{Expr, Quotes, ToExpr}
import scala.util.boundary
import scala.util.boundary.break

/**
 * Opaque view over a [[Regex]] exposing Brzozowski-derivative based language emptiness
 * and subset operations.
 *
 * `a.subset(b)` decides whether `L(a) ⊆ L(b)` by checking emptiness of `a ∩ ¬b`.
 * Termination relies on smart-constructor normalization in [[Regex]] keeping the
 * derivative state set finite up to similarity.
 *
 * [[Regex.Group]] nodes never survive lifting into this type: `Subset.of` erases every one of them
 * before the result is ever derived through (see that method's doc comment for why). That means
 * every other function in this file can - and does - assume no `Group` ever reaches it, which
 * matters specifically for `rawDerive`'s `Concat(Look(...), _)`/`Concat(Alt(...), _)`
 * shape-matching special cases below: a `Group` wrapping a leading `Look` (e.g. `((?=a))a`)
 * would otherwise have to be specifically unwrapped in each of those, on top of the ordinary
 * generic-recursion handling every other node already gets for free.
 */
opaque type Subset = Regex

/**
 * Raised (as a `Left`, never thrown) by [[Subset.isEmptyBounded]], [[Subset.subsetBounded]],
 * [[TokenMatcher.fromRegexesBounded]], and [[TokenMatcher.fromSubsetsBounded]] when deciding the
 * answer would require visiting more than `limit` distinct derivative states.
 *
 * This engine has no classic backtracking, so it isn't exposed to catastrophic-backtracking
 * ReDoS in the traditional sense - but the derivative state space these operations explore has
 * no bound of its own: a pattern built to maximize distinct derivative states (many overlapping
 * alternations/intersections/bounded repetitions near [[Regex.maxRepeatBound]]) can still make
 * either slow or memory-heavy. That matters wherever pattern strings come from untrusted input,
 * e.g. `Subset`'s stated use case of validating one pattern against another at load/build time.
 * The bounded methods this guards are opt-in: the plain `isEmpty`/`subset`/`fromRegexes`/
 * `fromSubsets` stay uncapped, unchanged, for every existing caller.
 */
final case class StateSpaceLimitExceeded(limit: Int):
  override def toString: String = s"derivative state-space limit ($limit distinct states) exceeded"

object Subset:

  /**
   * Lifts an existing [[Regex]] into [[Subset]], erasing every [[Regex.Group]] wrapper it
   * contains along the way ([[Regex.Group]]'s own doc comment covers why that's a safe,
   * language-preserving transform: `L(Group(_, _, inner)) = L(inner)` exactly). Capturing
   * groups matter to code that walks a `Regex` tree directly (the parser, a future capture-span
   * extractor) - they're invisible to every containment/matching question this type answers,
   * so there's no reason for any of `rawDerive`/`stripStartAnchor`/`hasLeadingLook` below to
   * know `Group` exists at all, and erasing here guarantees they never have to find out.
   */
  def of(r: Regex): Subset = eraseGroups(r)

  /**
   * See [[of]]. `deepRecursive`, same as `rawDerive`: a pattern built entirely out of nested
   * groups - `(((((...)))))` - recurses through this function exactly as deep as `rawDerive`
   * would recurse deriving the equivalent group-free pattern, so this needs the same heap-based
   * trampoline for stack safety, for the same reason.
   */
  private def eraseGroups(r: Regex): Regex = deepRecursive:
    r match
      case Eps | Empty | Chars(_) | StartAnchor => r
      case Concat(a, b) => eraseGroups(a).concat(eraseGroups(b))
      case Alt(parts) => Regex.alt(parts.toList.map(eraseGroups))
      case Inter(parts) => Regex.inter(parts.map(eraseGroups))
      case Star(inner) => eraseGroups(inner).star
      case Repeat(inner, lo, hi) => eraseGroups(inner).repeat(lo, hi)
      case Compl(inner) => !eraseGroups(inner)
      case Look(inner, positive) => Regex.lookahead(eraseGroups(inner), positive)
      case Group(_, _, inner) => eraseGroups(inner)

  /** Parses a pattern into a [[Subset]]. */
  def parse(pattern: String): Either[RegexParseError, Subset] = RegexParser.parse(pattern).map(of)

  /** The empty-language subset; reference-equal to [[Regex.Empty]] under the opaque type. */
  val empty: Subset = of(Regex.Empty)

  extension (a: Subset)
    /** Underlying [[Regex]]. */
    def underlying: Regex = a

    /** `Σ*`-extended view — matches every string having `a` as prefix. */
    def withAnySuffix: Subset = a.concat(Regex.all)

    /** `true` iff `L(a) ⊆ L(b)`. */
    def subset(b: Subset): Boolean = (a & !b).isEmpty

    /**
     * Like [[subset]], but fails fast with [[StateSpaceLimitExceeded]] instead of letting the
     * underlying [[isEmptyBounded]] BFS visit more than `maxStates` distinct derivative states -
     * see [[StateSpaceLimitExceeded]] for why that matters on untrusted patterns.
     */
    def subsetBounded(b: Subset, maxStates: Int): Either[StateSpaceLimitExceeded, Boolean] = (a & !b).isEmptyBounded(
      maxStates,
    )

    /** `true` iff `L(a) ⊆ L(b)` and `L(a) ≠ L(b)`. */
    def properSubset(b: Subset): Boolean = a.subset(b) && !b.subset(a)

    /** `true` iff `L(a) = ∅`. */
    def isEmpty: Boolean =
      @tailrec def loop(queue: Queue[Regex], visited: Set[Regex]): Boolean =
        queue.dequeueOption match
          case None => true
          case Some((s, rest)) =>
            if s.nullable then false
            else
              val derived = deriveAt(partitionReps(s), 0, s, Nil)
              val next = derived.filterNot(visited.contains)
              loop(rest.enqueueAll(next), visited ++ next)

      loop(Queue(a), Set(a))

    /**
     * Like [[isEmpty]], but fails fast with `Left(StateSpaceLimitExceeded(maxStates))` the
     * moment the BFS would need to have visited more than `maxStates` distinct derivative
     * states to keep going, instead of exploring an unbounded number of them. Opt-in guard for
     * callers deciding emptiness/subset (see [[subsetBounded]]) of a pattern that may come from
     * untrusted input - see [[StateSpaceLimitExceeded]].
     */
    def isEmptyBounded(maxStates: Int): Either[StateSpaceLimitExceeded, Boolean] = boundary:
      @tailrec def loop(queue: Queue[Regex], visited: Set[Regex]): Boolean =
        if visited.size > maxStates then break(Left(StateSpaceLimitExceeded(maxStates)))
        queue.dequeueOption match
          case None => true
          case Some((s, rest)) =>
            if s.nullable then false
            else
              val derived = deriveAt(partitionReps(s), 0, s, Nil)
              val next = derived.filterNot(visited.contains)
              loop(rest.enqueueAll(next), visited ++ next)

      Right(loop(Queue(a), Set(a)))

    /** `true` iff `ε ∈ L(a)`. */
    def nullable: Boolean = a.nullable

    /**
     * Brzozowski derivative of `a` with respect to code point `c`. Wrapped with
     * `stripStartAnchor`: whatever `rawDerive` returns represents "having consumed `c` from
     * `a`", meaning at least one character has now been consumed *somewhere* in the whole
     * match, so any `^`/`\A` reachable in that raw result — even one that was never itself
     * touched by this step, e.g. because it sat untouched inside an undivided sibling — has to
     * die. See `stripStartAnchor`'s doc for why this can't be handled locally the way `Look` is.
     */
    def derive(c: Int): Subset = stripStartAnchor(a.rawDerive(c))

    private def rawDerive(c: Int): Subset = deepRecursive:
      a match
        case Eps | Empty => Empty
        case Chars(set) => if set.contains(c) then Eps else Empty
        case StartAnchor => Empty

        /**
         * A leading lookahead can't be derived independently of what follows it - `Look(r,
         * _).derive` alone is always `Empty` (see below), which would silently drop the case
         * where `r` isn't satisfiable yet but becomes so after consuming `c`. So `r`'s own
         * derivative has to be threaded alongside `b`'s: `r.derive(c).concat(Regex.all)` is
         * exactly "does the remaining string (after `c`) have a prefix satisfying what's left
         * of `r`", which is intersected into `b`'s residual - `withAnySuffix` written out
         * inline since `Subset.withAnySuffix` isn't in scope for a bare `Regex`.
         */
        case Concat(Look(r, positive), b) =>
          val bc = b.derive(c)
          if positive then if r.nullable then bc else bc & r.derive(c).concat(Regex.all)
          else if r.nullable then Empty
          else bc & !r.derive(c).concat(Regex.all)

        /**
         * `(A|B)·b` is ordinarily fine to derive via the generic `Concat` rule below - it's
         * equivalent to `A·b | B·b` for ordinary regex, since `D_c` distributes over `|` either
         * way. But when a branch is itself a leading lookahead, that equivalence is the only
         * route to a correct answer: the generic rule only ever consults `Alt(parts).nullable`
         * and `Alt(parts).derive(c)`, both computed branch-independently - `Look(r,_).derive(c)`
         * is unconditionally `Empty` (see below), which would silently discard exactly the case
         * above exists to handle (the assertion becoming satisfiable only after consuming `c`).
         * Redistributing first, so each branch reaches the `Concat(Look(...), _)` case above
         * (or the ordinary rule, for a non-lookahead branch) on its own, avoids that loss. Only
         * taken when a branch actually needs it, to leave ordinary alternations (the overwhelming
         * majority) on the cheaper generic path below, sharing `b` instead of duplicating it.
         */
        case Concat(Alt(parts), b) if parts.exists(hasLeadingLook) =>
          Regex.alt(parts.toVector.map(_.concat(b).derive(c)))
        case Concat(a, b) =>
          val acb = a.derive(c).concat(b)
          if a.nullable then acb | b.derive(c)
          else acb
        case Alt(parts) => Regex.alt(parts.toVector.map(_.derive(c)))
        case Inter(parts) => Regex.inter(parts.map(_.derive(c)))
        case s @ Star(inner) => inner.derive(c).concat(s)

        /**
         * Standard counting-automaton derivative rule, taken directly through the symbolic
         * `Repeat` node instead of first unfolding it: `r{lo,hi} ≡ r · r{max(lo-1,0), hi-1}`
         * (`r{0,hi} ≡ Eps | r{1,hi}` collapses to the same shape, since `D_c(Eps) = Empty` kills
         * the "stop now" branch as soon as a character's actually been consumed). `hi` strictly
         * decreases every step and bottoms out at `r.repeat(_, 0) == Eps` (see `Regex.repeat`),
         * so this terminates the same way `Star`'s self-referential rule above does.
         */
        case Repeat(inner, lo, hi) =>
          val newHi = if hi == Int.MaxValue then Int.MaxValue else hi - 1
          inner.derive(c).concat(inner.repeat(math.max(lo - 1, 0), newHi))

        case Compl(inner) => !inner.derive(c)

        /** Zero-width: `L(Look(r, _)) ⊆ {ε}`, so no nonempty string can start it. */
        case Look(_, _) => Empty

        /** Never reached: [[Subset.of]] erases every `Group` before anything reaches here. */
        case g: Group => throw MatchError(s"unreachable: Subset never sees Group nodes (erased by Subset.of): $g")

  /**
   * `reps` is `Array[Int]`, not `List[Int]`: `List[Int]` boxes every element as a
   * `java.lang.Integer` (Scala's immutable `List` isn't specialized for primitives), and
   * `partitionReps` below rebuilds this collection fresh on every BFS state visited by
   * `isEmptyImpl`/`subset`. `Array[Int]` is an unboxed primitive `int[]` on the JVM, so
   * indexed iteration here allocates zero wrapper objects for the representatives.
   */
  @tailrec
  private def deriveAt(reps: Array[Int], idx: Int, r: Regex, acc: List[Regex]): List[Regex] =
    if idx >= reps.length then acc
    else deriveAt(reps, idx + 1, r, r.derive(reps(idx)) :: acc)

  /**
   * `true` iff `r` is a lookahead, or a (right-associated, per the `concat` smart constructor)
   * chain of concatenations headed by one - i.e. iff `r.concat(b)` would need the
   * `Concat(Look(...), _)` handling above rather than the ordinary `Concat` rule.
   */
  @tailrec
  private def hasLeadingLook(r: Regex): Boolean = r match
    case Look(_, _) => true
    case Concat(a, _) => hasLeadingLook(a)
    case Group(_, _, _) =>
      throw MatchError(s"unreachable: Subset never sees Group nodes (erased by Subset.of): $r")
    case _ => false

  /**
   * Collapses every `^`/`\A` (`StartAnchor`) reachable in `r` to `Empty`. Applied
   * unconditionally to the result of every `derive` call (see there): a derivative's result
   * represents "having consumed a character", so `^`/`\A` can never hold in it again anywhere —
   * even for an occurrence that this specific derivative step never itself touched, e.g. one
   * left untouched inside an undivided sibling. That's why this can't be a local rule the way
   * `Look`'s `Concat`-head handling is: `Look`'s continuation is threaded explicitly through the
   * one node deriving it, but `^`/`\A` has to die *globally*, in parts of the tree the current
   * derivative step never visits at all — a single recursive sweep over the whole result is the
   * simplest way to guarantee that. `hasStartAnchor` keeps this a single cheap check (not a tree
   * walk) for the overwhelming majority of patterns that never use `^`/`\A` - so only a pattern
   * that actually contains one ever pays for the `deepRecursive` trampoline below.
   */
  private def stripStartAnchor(r: Regex): Regex =
    if !r.hasStartAnchor then r
    else
      deepRecursive:
        r match
          case StartAnchor => Empty
          case Concat(a, b) => stripStartAnchor(a).concat(stripStartAnchor(b))
          case Alt(parts) => Regex.alt(parts.toVector.map(stripStartAnchor))
          case Inter(parts) => Regex.inter(parts.map(stripStartAnchor))
          case Star(inner) => stripStartAnchor(inner).star
          case Repeat(inner, lo, hi) => stripStartAnchor(inner).repeat(lo, hi)
          case Compl(inner) => !stripStartAnchor(inner)
          case Look(inner, positive) => Regex.lookahead(stripStartAnchor(inner), positive)
          case g: Group => throw MatchError(s"unreachable: Subset never sees Group nodes (erased by Subset.of): $g")
          case _ => r

  /**
   * Returns one representative code point per equivalence class of the alphabet
   * partition induced by the character sets in `r`. Within a class, derivatives
   * yield the same residual, so testing one representative suffices.
   */
  private def partitionReps(r: Regex): Array[Int] =
    (SortedSet(0, CharSet.maxCodePoint + 1) ++ r.alphabetBoundaries).init.toArray

  // $COVERAGE-OFF$
  given ToExpr[Subset]:
    def apply(s: Subset)(using Quotes): Expr[Subset] = '{ Subset.of(${ Expr(s.underlying) }) }
  // $COVERAGE-ON$
