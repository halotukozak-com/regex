package halotukozak.regex

class TokenMatcherTest extends munit.FunSuite:

  private def parse(pattern: String): Regex = RegexParser.parse(pattern) match
    case Right(r) => r
    case Left(err) => fail(s"expected successful parse of /$pattern/, got $err")

  private def matcher(patterns: String*): TokenMatcher = TokenMatcher.fromRegexes(patterns.map(parse)*)

  test("matches the longest prefix across patterns") {
    val m = matcher("if", "[a-zA-Z_][a-zA-Z0-9_]*")
    assertEquals(m.matchAt("ifx", 0), Some((priority = 1, end = 3)))
  }

  test("ties broken by lowest priority index (earlier pattern wins)") {
    val m = matcher("if", "[a-zA-Z_][a-zA-Z0-9_]*")
    assertEquals(m.matchAt("if", 0), Some((priority = 0, end = 2)))
  }

  test("matches at a non-zero start offset") {
    val m = matcher("[a-z]+")
    assertEquals(m.matchAt("12abc", 2), Some((priority = 0, end = 5)))
  }

  test("returns None when no pattern matches even an empty prefix") {
    val m = matcher("[a-z]+")
    assertEquals(m.matchAt("123", 0), None)
  }

  test("matches a zero-length pattern (e.g. from a? or a*)") {
    val m = matcher("a*")
    assertEquals(m.matchAt("bbb", 0), Some((priority = 0, end = 0)))
  }

  test("findFirst locates the first non-empty match at or after `from`") {
    val m = matcher("[a-z]+")
    assertEquals(m.findFirst("123abc456", 0), Some((start = 3, priority = 0, end = 6)))
  }

  test("findFirst returns None when no non-empty match exists at or after `from`") {
    val m = matcher("[a-z]+")
    assertEquals(m.findFirst("12345", 0), None)
  }

  test("findFirst respects the `from` lower bound") {
    val m = matcher("[a-z]+")
    assertEquals(m.findFirst("abc123def", 4), Some((start = 6, priority = 0, end = 9)))
  }

  test("multiple patterns racing in parallel, correct one wins by length then priority") {
    val m = matcher("\\d+", "\\d+\\.\\d+")
    assertEquals(m.matchAt("3.14", 0), Some((priority = 1, end = 4)))
    assertEquals(m.matchAt("314", 0), Some((priority = 0, end = 3)))
  }
