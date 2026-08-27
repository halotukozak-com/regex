package halotukozak.regex

class CaptureTest extends munit.FunSuite:

  private def m(pattern: String): CaptureMatcher = CaptureMatcher.parse(pattern) match
    case Right(matcher) => matcher
    case Left(err) => fail(s"expected successful parse of /$pattern/, got $err")

  private def matchOf(pattern: String, input: String): MatchResult =
    m(pattern).matchWhole(input) match
      case Some(result) => result
      case None => fail(s"expected /$pattern/ to match \"$input\"")

  // group spans

  test("two sibling groups") {
    val result = matchOf("(a)(b)", "ab")
    assertEquals(result.group(1), Some((start = 0, end = 1)))
    assertEquals(result.group(2), Some((start = 1, end = 2)))
    assertEquals(result.group(0), Some((start = 0, end = 2)))
  }

  test("optional group that never participates is None, not a zero-width span") {
    val result = matchOf("(a)?", "")
    assertEquals(result.group(1), None)
  }

  test("group that participates and captures zero characters is Some((p,p)), not None") {
    val result = matchOf("(a?)", "")
    assertEquals(result.group(1), Some((start = 0, end = 0)))
  }

  test("alternation: only the taken branch's group is set") {
    val result = matchOf("(a)|(b)", "b")
    assertEquals(result.group(1), None)
    assertEquals(result.group(2), Some((start = 0, end = 1)))
  }

  test("leftmost-first alternation priority (Cox's canonical a|ab example)") {
    val result = matchOf("(a|ab)(c|bcd)(d*)", "abcd")
    assertEquals(result.group(1), Some((start = 0, end = 1)))
    assertEquals(result.group(2), Some((start = 1, end = 4)))
    assertEquals(result.group(3), Some((start = 4, end = 4)))
  }

  test("group inside a star keeps only the last iteration's span") {
    val result = matchOf("((a)(b))*", "abab")
    assertEquals(result.group(1), Some((start = 2, end = 4)))
    assertEquals(result.group(2), Some((start = 2, end = 3)))
    assertEquals(result.group(3), Some((start = 3, end = 4)))
  }

  test("group inside a star that runs zero times is None") {
    val result = matchOf("(a)*", "")
    assertEquals(result.group(1), None)
  }

  test("named groups resolve to the right indices") {
    val result = matchOf("(?<year>\\d{4})-(?<month>\\d{2})", "2026-08")
    assertEquals(result.group("year"), Some((start = 0, end = 4)))
    assertEquals(result.group("month"), Some((start = 5, end = 7)))
    assertEquals(result.group("year"), result.group(1))
    assertEquals(result.group("month"), result.group(2))
  }

  test("group nested inside a lookahead still captures") {
    val result = matchOf("(?=(?<x>a))a", "a")
    assertEquals(result.group("x"), Some((start = 0, end = 1)))
    assertEquals(result.group(0), Some((start = 0, end = 1)))
  }

  test("negative lookahead never contributes captures") {
    val result = matchOf("(?!(?<x>b))a", "a")
    assertEquals(result.group("x"), None)
  }

  // whole-string match semantics

  test("no match returns None") {
    assertEquals(m("(a)(b)").matchWhole("ac"), None)
    assertEquals(m("(a)(b)").matchWhole("a"), None)
    assertEquals(m("(a)(b)").matchWhole("abc"), None)
  }

  test("bounded repetition on a group") {
    val result = matchOf("(ab){2,3}", "ababab")
    assertEquals(result.group(1), Some((start = 4, end = 6)))
  }

  // parse errors

  test("parse error surfaces as Left") {
    CaptureMatcher.parse("(a") match
      case Left(_) => ()
      case Right(_) => fail("expected a parse error for an unterminated group")
  }

  // unapplySeq - `case myMatcher(g1, g2) => ...`, per issue #52

  test("unapplySeq lets a CaptureMatcher be used as a case pattern") {
    val date = m("""(\d{4})-(\d{2})-(\d{2})""")
    "2026-08-27" match
      case date(year, month, day) => assertEquals((year, month, day), (Some("2026"), Some("08"), Some("27")))
      case _ => fail("expected the date pattern to match")
  }

  test("unapplySeq: non-participating group is None, not null-in-a-List like scala.util.matching.Regex") {
    val m2 = m("(a)|(b)")
    "b" match
      case m2(g1, g2) => assertEquals((g1, g2), (None, Some("b")))
      case _ => fail("expected the alternation to match")
  }

  test("unapplySeq: no match is None, doesn't throw") {
    assertEquals(m("(a)(b)").unapplySeq("ac"), None)
  }

  test("unapplySeq excludes group 0 (the whole match), same convention as scala.util.matching.Regex") {
    assertEquals(m("(a)(b)").unapplySeq("ab"), Some(Seq(Some("a"), Some("b"))))
  }
