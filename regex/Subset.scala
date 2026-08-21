package halotukozak.regex

import halotukozak.regex.Regex.*

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, SortedSet}
import scala.util.control.TailCalls.{done, tailcall, TailRec}

/**
 * Opaque view over a [[Regex]] exposing Brzozowski-derivative based language emptiness
 * and subset operations.
 *
 * `a.subset(b)` decides whether `L(a) ⊆ L(b)` by checking emptiness of `a ∩ ¬b`.
 * Termination relies on smart-constructor normalization in [[Regex]] keeping the
 * derivative state set finite up to similarity.
 */
opaque type Subset = Regex

object Subset:

  /** Lifts an existing [[Regex]] into [[Subset]]. */
  def of(r: Regex): Subset = r

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

    /** `true` iff `L(a) ⊆ L(b)` and `L(a) ≠ L(b)`. */
    def properSubset(b: Subset): Boolean = a.subset(b) && !b.subset(a)

    /** `true` iff `L(a) = ∅`. */
    def isEmpty: Boolean = isEmptyImpl(a)

    /** `true` iff `ε ∈ L(a)`. */
    def nullable: Boolean = a.nullable

    /** Brzozowski derivative of `a` with respect to code point `c`. */
    def derive(c: Int): Subset = deriveImpl(a, c).result

  private def isEmptyImpl(r: Regex): Boolean =
    def loop(queue: Queue[Regex], visited: Set[Regex]): TailRec[Boolean] =
      queue.dequeueOption match
        case None => done(true)
        case Some((s, rest)) =>
          if s.nullable then done(false)
          else
            val derived = deriveAt(partitionReps(s), s, Nil)
            val next = derived.filterNot(visited.contains)
            tailcall(loop(rest.enqueueAll(next), visited ++ next))
    loop(Queue(r), Set(r)).result

  private def deriveImpl(r: Regex, c: Int): TailRec[Regex] = r match
    case Eps | Empty => done(Empty)
    case Chars(set) => done(if set.contains(c) then Eps else Empty)
    case Concat(a, b) =>
      for
        da <- tailcall(deriveImpl(a, c))
        out <-
          if a.nullable then tailcall(deriveImpl(b, c)).map(db => da.concat(b) | db)
          else done(da.concat(b))
      yield out
    case Alt(parts) => done(Regex.alt(deriveAll(parts.toList, c, Nil)))
    case Inter(parts) => done(Regex.inter(deriveAll(parts.toList, c, Nil)))
    case s @ Star(inner) => tailcall(deriveImpl(inner, c)).map(d => d.concat(s))
    case Compl(inner) => tailcall(deriveImpl(inner, c)).map(!_)

  /**
   * Plain `@tailrec` loops, not `TailRec`-trampolined: unlike `deriveImpl`'s tree recursion
   * (whose depth tracks regex structure and needs the heap-based trampoline for stack
   * safety), these only walk a flat list — `parts`/`reps` — whose length is bounded by
   * branch/partition count, not tree depth. Composing them through `tailcall`/`for` would
   * still be stack-safe but pays for a `Cont` continuation-object allocation per list
   * element; forcing each `deriveImpl(...).result` eagerly here avoids that allocation.
   *
   * Trade-off: each forced `.result` is a real (non-tail) JVM call into `deriveImpl`, so an
   * `Alt`/`Inter` node nested inside another `Alt`/`Inter`'s part now grows the JVM call
   * stack by one frame per nesting level, instead of being folded into the single top-level
   * trampoline as before. Verified this stays within the JVM's minimum stack size
   * (`-Xss208k`) for the worst case the library's own `maxRepeatBound` allows (one `{0,1000}`
   * quantifier); deliberately-adversarial nesting far beyond that bound can still overflow —
   * same pre-existing ceiling `Regex#hashCode`/`nullable`/`alphabetBoundaries` already have,
   * since those are also plain structural recursion, not trampolined.
   */
  @tailrec
  private def deriveAll(parts: List[Regex], c: Int, acc: List[Regex]): List[Regex] = parts match
    case Nil => acc
    case head :: tail => deriveAll(tail, c, deriveImpl(head, c).result :: acc)

  @tailrec
  private def deriveAt(reps: List[Int], r: Regex, acc: List[Regex]): List[Regex] = reps match
    case Nil => acc
    case c :: tail => deriveAt(tail, r, deriveImpl(r, c).result :: acc)

  /**
   * Returns one representative code point per equivalence class of the alphabet
   * partition induced by the character sets in `r`. Within a class, derivatives
   * yield the same residual, so testing one representative suffices.
   */
  private def partitionReps(r: Regex): List[Int] =
    (SortedSet(0, CharSet.maxCodePoint + 1) ++ r.alphabetBoundaries).init.toList
