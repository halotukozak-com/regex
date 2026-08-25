package halotukozak.regex

import halotukozak.regex.Regex.*

class RegexAlgebraTest extends munit.FunSuite:

  private val rA = Regex.lit('a')
  private val rB = Regex.lit('b')
  private val rC = Regex.lit('c')

  // concat ----------------------------------------------------------------

  test("concat with Empty annihilates") {
    assertEquals(Empty.concat(rA), Empty)
    assertEquals(rA.concat(Empty), Empty)
  }

  test("concat with Eps is identity") {
    assertEquals(Eps.concat(rA), rA)
    assertEquals(rA.concat(Eps), rA)
  }

  test("concat is right-associated") {
    rA.concat(rB).concat(rC) match
      case Concat(a, Concat(b, c)) => assertEquals((a, b, c), (rA, rB, rC))
      case other => fail(s"not right-associated: $other")
  }

  // alt -------------------------------------------------------------------

  test("alt with Empty drops it") {
    assertEquals(Empty | rA, rA)
    assertEquals(rA | Empty, rA)
  }

  test("alt deduplicates equal regexes") {
    assertEquals(rA | rA, rA)
  }

  test("alt flattens nested Alt") {
    val sA = rA.star
    val sB = rB.star
    val sC = rC.star
    sA | sB | sC match
      case Alt(parts) => assertEquals(parts, Set(sA, sB, sC))
      case other => fail(s"not flattened Alt: $other")
  }

  test("alt merges Chars by union") {
    val merged = Regex.lit('a') | Regex.lit('b')
    assertEquals(merged, Regex(CharSet.normalize(Range('a'.toInt, 'a'.toInt), Range('b'.toInt, 'b'.toInt))))
  }

  // inter -----------------------------------------------------------------

  test("inter with Empty gives Empty") {
    assertEquals(Empty & rA, Empty)
    assertEquals(rA & Empty, Empty)
  }

  test("inter deduplicates equal regexes") {
    assertEquals(rA & rA, rA)
  }

  test("inter intersects Chars") {
    val ab = Regex.range('a', 'm')
    val bc = Regex.range('h', 'z')
    assertEquals(ab & bc, Regex.range('h', 'm'))
  }

  test("inter of disjoint Chars is Empty") {
    assertEquals(Regex.lit('a') & Regex.lit('b'), Empty)
  }

  // star ------------------------------------------------------------------

  test("star of Eps is Eps") {
    assertEquals(Eps.star, Eps)
  }

  test("star of Empty is Eps") {
    assertEquals(Empty.star, Eps)
  }

  test("star of Star is the same Star") {
    val s = rA.star
    assertEquals(s.star, s)
  }

  // compl -----------------------------------------------------------------

  test("compl is involutive") {
    assertEquals(!(!rA), rA)
  }

  // lookahead ---------------------------------------------------------------

  test("lookahead wraps a non-degenerate body") {
    Regex.lookahead(rA, positive = true) match
      case Look(r, positive) => assertEquals((r, positive), (rA, true))
      case other => fail(s"not Look: $other")
  }

  test("positive lookahead of Empty is Empty; of Eps is Eps") {
    assertEquals(Regex.lookahead(Empty, positive = true), Empty)
    assertEquals(Regex.lookahead(Eps, positive = true), Eps)
  }

  test("negative lookahead of Empty is Eps; of Eps is Empty") {
    assertEquals(Regex.lookahead(Empty, positive = false), Eps)
    assertEquals(Regex.lookahead(Eps, positive = false), Empty)
  }

  // literal ---------------------------------------------------------------

  test("literal of empty string is Eps") {
    assertEquals(Regex.literal(""), Eps)
  }

  test("literal of single char is Chars") {
    assertEquals(Regex.literal("a"), Regex.lit('a'))
  }

  test("literal of two chars is right-associated Concat") {
    Regex.literal("ab") match
      case Concat(a, b) => assertEquals((a, b), (Regex.lit('a'), Regex.lit('b')))
      case other => fail(s"not Concat: $other")
  }

  // repeat ----------------------------------------------------------------

  test("repeat {0} is Eps") {
    assertEquals(rA.repeat(0, 0), Eps)
  }

  test("repeat {1,1} is the regex itself") {
    assertEquals(rA.repeat(1, 1), rA)
  }

  test("repeat {n, MaxValue} ends with Star") {
    assertEquals(rA.repeat(2, Int.MaxValue), rA.concat(rA).concat(rA.star))
  }

  test("repeat rejects invalid bounds") {
    intercept[IllegalArgumentException](rA.repeat(-1, 0))
    intercept[IllegalArgumentException](rA.repeat(3, 2))
  }

  test("repeat rejects bounds above maxRepeatBound") {
    intercept[IllegalArgumentException](rA.repeat(0, Regex.maxRepeatBound + 1))
    intercept[IllegalArgumentException](rA.repeat(Regex.maxRepeatBound + 1, Regex.maxRepeatBound + 1))
  }
