package halotukozak.regex

import halotukozak.regex.Regex.*

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, SortedSet}
import scala.quoted.{Expr, Quotes, ToExpr}
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
            val derived = deriveAt(partitionReps(s), 0, s, Nil)
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
    else deriveAt(reps, idx + 1, r, deriveImpl(r, reps(idx)).result :: acc)

  /**
   * Returns one representative code point per equivalence class of the alphabet
   * partition induced by the character sets in `r`. Within a class, derivatives
   * yield the same residual, so testing one representative suffices.
   */
  private def partitionReps(r: Regex): Array[Int] =
    (SortedSet(0, CharSet.maxCodePoint + 1) ++ r.alphabetBoundaries).init.toArray

  // $COVERAGE-OFF$
  /**
   * Routed through `Subset.of` rather than splicing `Expr(s.underlying): Expr[Regex]` directly:
   * a tree's static type, once spliced into a foreign compilation unit, is whatever its
   * outermost call is *declared* to return - opaque-type transparency only holds lexically
   * inside `Subset`'s own defining scope, at the point this `Expr` gets built, not at the
   * (unrelated, external) point it's later typechecked after splicing. `Subset.of`'s declared
   * signature returns `Subset`, so the call is externally `Subset`-typed no matter where the
   * resulting tree lands - unlike a bare `Regex`-returning tree, which would need every splice
   * site to already share this scope's transparency to typecheck as `Subset`.
   */
  given ToExpr[Subset]:
    def apply(s: Subset)(using Quotes): Expr[Subset] = '{ Subset.of(${ Expr(s.underlying) }) }
  // $COVERAGE-ON$
