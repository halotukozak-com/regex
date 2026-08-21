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

  // ---------------------------------------------------------------------------------------
  // Adapted from real lexer/tokenizer implementations to cross-check maximal-munch +
  // priority-tiebreak semantics: longest match wins across *all* rules; among matches of
  // equal length, the first-declared (lowest-priority-index) rule wins.
  //
  // Note on scope: moo (JS, github.com/no-context/moo) only auto-sorts-by-length *within*
  // one rule's own literal alternatives - across different rules it just tries them in
  // declaration order regardless of match length (its own docs call this out: `{one: 'moo',
  // two: 'moomintroll'}` matched against "moomintroll" yields "moo", not the longer match).
  // That's the opposite of what TokenMatcher does, so it isn't ported here - true
  // cross-rule maximal munch is JFlex's (and javac/kotlinc's JFlex-based lexers') semantics,
  // which is what TokenMatcher actually implements.
  // ---------------------------------------------------------------------------------------

  test("moo: same-length match ties are broken by declaration order (README.md)") {
    // moo.compile({ identifier: /[a-z0-9]+/, number: /[0-9]+/ }).reset('42').next()
    //   -> { type: 'identifier', value: '42' }
    val identifierFirst = matcher("[a-z0-9]+", "[0-9]+")
    assertEquals(identifierFirst.matchAt("42", 0), Some((priority = 0, end = 2)))

    // moo.compile({ number: /[0-9]+/, identifier: /[a-z0-9]+/ }).reset('42').next()
    //   -> { type: 'number', value: '42' }
    val numberFirst = matcher("[0-9]+", "[a-z0-9]+")
    assertEquals(numberFirst.matchAt("42", 0), Some((priority = 0, end = 2)))
  }

  test("moo/kotlinc: keyword ties with an identifier prefix, but a longer identifier wins (test.js, Kotlin.flex)") {
    // moo's motivating example for keywords-as-a-subset-of-identifier (test.js, README.md):
    // naive `{keyword: ['class'], identifier: /[a-zA-Z]+/}` still needs `class` to win on a
    // tie, and `identifier` to win once the match is strictly longer.
    val m = matcher("class", "[a-zA-Z]+")
    assertEquals(m.matchAt("class", 0), Some((priority = 0, end = 5)))
    assertEquals(m.matchAt("className", 0), Some((priority = 1, end = 9)))
  }

  test("jflex: a longer literal that fails partway through contributes no match at all (performance.md)") {
    // JFlex's own docs illustrate maximal munch with `averylongkeyword` vs `.` against input
    // "averylongjoke": the keyword shares a 9-char prefix ("averylong") with the input before
    // diverging at 'k' vs 'j'. A literal pattern only accepts at its exact full length, so
    // that dead-end prefix contributes nothing - only `.` (1 char) matches.
    val m = matcher("averylongkeyword", ".")
    assertEquals(m.matchAt("averylongjoke", 0), Some((priority = 1, end = 1)))
  }
