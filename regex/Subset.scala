package halotukozak.regex

import halotukozak.regex.Regex.*

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
            for
              derived <- tailcall(deriveAt(partitionReps(s), s, Nil))
              next = derived.filterNot(visited.contains)
              r <- tailcall(loop(rest.enqueueAll(next), visited ++ next))
            yield r
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
    case Alt(parts) => deriveAll(parts.toList, c, Nil).map(Regex.alt)
    case Inter(parts) => deriveAll(parts.toList, c, Nil).map(Regex.inter)
    case s @ Star(inner) => tailcall(deriveImpl(inner, c)).map(d => d.concat(s))
    case Compl(inner) => tailcall(deriveImpl(inner, c)).map(!_)

  private def deriveAll(parts: List[Regex], c: Int, acc: List[Regex]): TailRec[List[Regex]] = parts match
    case Nil => done(acc)
    case head :: tail =>
      for
        d <- tailcall(deriveImpl(head, c))
        out <- tailcall(deriveAll(tail, c, d :: acc))
      yield out

  private def deriveAt(reps: List[Int], r: Regex, acc: List[Regex]): TailRec[List[Regex]] = reps match
    case Nil => done(acc)
    case c :: tail =>
      for
        d <- tailcall(deriveImpl(r, c))
        out <- tailcall(deriveAt(tail, r, d :: acc))
      yield out

  /**
   * Returns one representative code point per equivalence class of the alphabet
   * partition induced by the character sets in `r`. Within a class, derivatives
   * yield the same residual, so testing one representative suffices.
   */
  private def partitionReps(r: Regex): List[Int] =
    (SortedSet(0, CharSet.maxCodePoint + 1) ++ r.alphabetBoundaries).init.toList
