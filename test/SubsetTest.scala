package halotukozak.regex

import halotukozak.regex.Regex.*

class SubsetTest extends munit.FunSuite:

  private def s(pattern: String): Subset = Subset.parse(pattern) match
    case Right(sub) => sub
    case Left(err) => fail(s"expected successful parse of /$pattern/, got $err")
  private def s(r: Regex): Subset = Subset.of(r)

  test("identity: r ⊆ r") {
    assert(s("a").subset(s("a")))
    assert(s("[a-z]").subset(s("[a-z]")))
    assert(s("if").subset(s("if")))
  }

  test("empty language is subset of anything") {
    assert(s(Empty).subset(s("a")))
    assert(s(Empty).subset(s(Empty)))
  }

  test("nothing nonempty is subset of empty") {
    assert(!s("a").subset(s(Empty)))
  }

  test("Eps ⊆ a*") {
    assert(s(Eps).subset(s("a*")))
  }

  test("strict subset: a ⊆ [a-z]") {
    assert(s("a").subset(s("[a-z]")))
    assert(!s("[a-z]").subset(s("a")))
  }

  test("prefix subset via .* extension") {
    assert(s("if.*").subset(s("i.*")))
    assert(!s("i.*").subset(s("if.*")))
  }

  test("character class subset") {
    assert(s("[a-c]").subset(s("[a-z]")))
    assert(!s("[a-z]").subset(s("[a-c]")))
  }

  test("alternation subset") {
    assert(s("a").subset(s("a|b")))
    assert(s("a|b").subset(s("a|b|c")))
    assert(!s("a|b|c").subset(s("a|b")))
  }

  test("Kleene star relationships") {
    assert(s("a").subset(s("a*")))
    assert(s("a*").subset(s(".*")))
  }

  test("isEmpty detects empty languages") {
    assert(s(Empty).isEmpty)
    assert(!s(Eps).isEmpty)
    assert(s(Regex(CharSet.empty)).isEmpty)
    assert(!s("a").isEmpty)
    assert(s(s("a").underlying & s("b").underlying).isEmpty)
    assert(s(s("[a-z]").underlying & s("[A-Z]").underlying).isEmpty)
    assert(!s(s("[a-m]").underlying & s("[h-z]").underlying).isEmpty)
  }

  test("complement reverses subset") {
    val a = s("[a-z]")
    val notA = s(!a.underlying)
    assert(!a.subset(notA))
    assert(s(a.underlying & notA.underlying).isEmpty)
  }

  // nullable --------------------------------------------------------------

  test("nullable: Eps and Star are nullable") {
    assert(s(Eps).nullable)
    assert(s("a*").nullable)
  }

  test("nullable: non-empty literals are not nullable") {
    assert(!s("a").nullable)
    assert(!s("abc").nullable)
  }

  test("nullable: Empty is not nullable") {
    assert(!s(Empty).nullable)
  }

  test("nullable: a? is nullable") {
    assert(s("a?").nullable)
  }

  test("nullable: alternation if any branch is") {
    assert(s("a|b*").nullable)
    assert(!s("a|b").nullable)
  }

  test("nullable: concat requires both nullable") {
    assert(s("a*b*").nullable)
    assert(!s("a*b").nullable)
  }

  // derive ----------------------------------------------------------------

  test("derive of literal 'a' wrt 'a' is Eps") {
    assertEquals(s("a").derive('a'.toInt).underlying, Eps)
  }

  test("derive of literal 'a' wrt 'b' is Empty") {
    assertEquals(s("a").derive('b'.toInt).underlying, Empty)
  }

  test("derive of 'ab' wrt 'a' yields 'b'") {
    assertEquals(s("ab").derive('a'.toInt).underlying, Regex.lit('b'))
  }

  test("derive of 'a*' wrt 'a' is 'a*'") {
    assertEquals(s("a*").derive('a'.toInt).underlying, Regex.lit('a').star)
  }

  test("derive of char class") {
    assertEquals(s("[a-z]").derive('m'.toInt).underlying, Eps)
    assertEquals(s("[a-z]").derive('A'.toInt).underlying, Empty)
  }

  // withAnySuffix ---------------------------------------------------------

  test("withAnySuffix accepts prefix matches") {
    val ext = s("if").withAnySuffix
    assert(ext.derive('i'.toInt).derive('f'.toInt).nullable)
    assert(ext.derive('i'.toInt).derive('f'.toInt).derive('x'.toInt).nullable)
  }

  test("withAnySuffix used in shadow check") {
    assert(s("if").withAnySuffix.subset(s("i").withAnySuffix))
    assert(!s("i").withAnySuffix.subset(s("if").withAnySuffix))
  }

  // lookahead ---------------------------------------------------------------

  test("nullable: standalone positive lookahead mirrors its body's nullability") {
    assert(s(Regex.lookahead(Regex.lit('a').star, positive = true)).nullable)
    assert(!s(Regex.lookahead(Regex.lit('a'), positive = true)).nullable)
  }

  test("nullable: standalone negative lookahead is the complement of its body's nullability") {
    assert(!s(Regex.lookahead(Regex.lit('a').star, positive = false)).nullable)
    assert(s(Regex.lookahead(Regex.lit('a'), positive = false)).nullable)
  }

  test("derive: a bare lookahead never consumes a character") {
    assertEquals(s(Regex.lookahead(Regex.lit('a'), positive = true)).derive('a'.toInt).underlying, Empty)
    assertEquals(s(Regex.lookahead(Regex.lit('a'), positive = false)).derive('a'.toInt).underlying, Empty)
  }

  test("derive: positive lookahead gates the concatenation that follows it") {
    val pattern = s(Regex.lookahead(Regex.lit('a'), positive = true).concat(Regex.lit('a')))
    assert(pattern.derive('a'.toInt).nullable)
    assert(pattern.derive('b'.toInt).isEmpty)
  }

  test("derive: negative lookahead excludes the string it names, even though the body would accept it") {
    val body = Regex.lit('a') | Regex.lit('b')
    val pattern = s(Regex.lookahead(Regex.lit('a'), positive = false).concat(body))
    assert(!pattern.derive('a'.toInt).nullable)
    assert(pattern.derive('b'.toInt).nullable)
  }

  test("subset: lookahead narrows a language without changing string length") {
    val plain = s("[ab]")
    val gated = s(Regex.lookahead(Regex.lit('a'), positive = true).concat(Regex(CharSet.normalize(Range('a', 'b')))))
    assert(gated.subset(plain))
    assert(!plain.subset(gated))
  }

  // parse / of constructors -----------------------------------------------

  test("Subset.parse and Subset.of compose") {
    assertEquals(s("abc").underlying, Regex.literal("abc"))
    assertEquals(Subset.of(Eps).underlying, Eps)
  }

  // isEmptyBounded / subsetBounded -----------------------------------------

  // Neither operand is a bare `Chars` node, so the smart constructors can't collapse this to
  // `Empty` at construction time (same reasoning as `disjointIntersection` in
  // `SubsetBenchmark`) - `isEmpty`'s BFS genuinely has to walk derivative states across the
  // whole `[a-z]*` middle section before concluding the language is empty, so it's a case that
  // needs more than a couple of visited states to decide either way.
  private lazy val disjointIntersection = s(s("a[a-z]*b").underlying & s("a[a-z]*c").underlying)

  test("isEmptyBounded agrees with isEmpty once the cap is generous enough") {
    assertEquals(disjointIntersection.isEmptyBounded(10_000), Right(disjointIntersection.isEmpty))
    assertEquals(s("a").isEmptyBounded(10_000), Right(s("a").isEmpty))
  }

  test("isEmptyBounded fails fast, without exploring further, once the cap is too small") {
    assertEquals(disjointIntersection.isEmptyBounded(1), Left(StateSpaceLimitExceeded(1)))
  }

  test("isEmptyBounded(0) always fails: even the start state alone exceeds a zero cap") {
    assertEquals(s(Empty).isEmptyBounded(0), Left(StateSpaceLimitExceeded(0)))
    assertEquals(s(Empty).isEmptyBounded(1), Right(true))
  }

  test("subsetBounded agrees with subset once the cap is generous enough") {
    assertEquals(s("a").subsetBounded(s("[a-z]"), 10_000), Right(true))
    assertEquals(s("[a-z]").subsetBounded(s("a"), 10_000), Right(false))
  }

  test("subsetBounded fails fast once the cap is too small") {
    assert(disjointIntersection.subsetBounded(s(Empty), 1).isLeft)
  }
