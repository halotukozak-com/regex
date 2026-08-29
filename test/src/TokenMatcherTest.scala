package halotukozak.regex

class TokenMatcherTest extends munit.FunSuite:

  private def parse(pattern: String): Regex = RegexParser.parse(pattern) match
    case Right(r) => r
    case Left(err) => fail(s"expected successful parse of /$pattern/, got $err")

  private def matcher(patterns: String*): TokenMatcher = TokenMatcher.fromRegexes(patterns.map(parse)*)

  test("matches the longest prefix across patterns") {
    val m = matcher("if", "[a-zA-Z_][a-zA-Z0-9_]*")
    assertEquals(m.matchAt("ifx", 0), (priority = 1, end = 3))
  }

  test("ties broken by lowest priority index (earlier pattern wins)") {
    val m = matcher("if", "[a-zA-Z_][a-zA-Z0-9_]*")
    assertEquals(m.matchAt("if", 0), (priority = 0, end = 2))
  }

  test("matches at a non-zero start offset") {
    val m = matcher("[a-z]+")
    assertEquals(m.matchAt("12abc", 2), (priority = 0, end = 5))
  }

  test("returns null when no pattern matches even an empty prefix") {
    val m = matcher("[a-z]+")
    assertEquals(m.matchAt("123", 0), null)
  }

  test("matches a zero-length pattern (e.g. from a? or a*)") {
    val m = matcher("a*")
    assertEquals(m.matchAt("bbb", 0), (priority = 0, end = 0))
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
    assertEquals(m.matchAt("3.14", 0), (priority = 1, end = 4))
    assertEquals(m.matchAt("314", 0), (priority = 0, end = 3))
  }

  // ---------------------------------------------------------------------------------------
  // fastAscii bypass (#462): matchAt's array-lookup shortcut for a single ASCII start
  // character whose one-character match is unambiguously the longest possible one. These
  // pin the *safety* condition directly, on top of the general longest-match cross-checks
  // above/below already exercising it indirectly (e.g. the `-` vs `->` and `.`/`..`/`..<`
  // cases) -- the correctness bar is that fastAscii must agree with matchAtSlow on every
  // input, not just be fast on the cases it was designed for.
  // ---------------------------------------------------------------------------------------

  test("fastAscii: pure single-char literals with no overlap all take the bypass and match correctly") {
    val m = matcher("\\{", "\\}", ",", ":", "\\(", "\\)")
    assertEquals(m.matchAt("{", 0), (priority = 0, end = 1))
    assertEquals(m.matchAt("}", 0), (priority = 1, end = 1))
    assertEquals(m.matchAt(",", 0), (priority = 2, end = 1))
    assertEquals(m.matchAt(":", 0), (priority = 3, end = 1))
    assertEquals(m.matchAt("(", 0), (priority = 4, end = 1))
    assertEquals(m.matchAt(")", 0), (priority = 5, end = 1))
  }

  test("fastAscii: does not take the bypass when a longer pattern shares the same first character") {
    val m = matcher("-", "->")
    // `-` alone: the bypass's "no live continuation" condition must be false here (state after
    // consuming '-' still has a live transition on '>'), so this must fall through to the
    // general walk and correctly return the 1-char match rather than bypass-guessing wrong.
    assertEquals(m.matchAt("-x", 0), (priority = 0, end = 1))
    // `->`: the general walk must still find the longer match; the bypass never overrides it.
    assertEquals(m.matchAt("->", 0), (priority = 1, end = 2))
  }

  test("fastAscii: single-char literal that is a prefix of a longer literal, both directions") {
    val m = matcher(":", "::")
    assertEquals(m.matchAt(":x", 0), (priority = 0, end = 1))
    assertEquals(m.matchAt("::", 0), (priority = 1, end = 2))
  }

  test("fastAscii: matching at the end of input returns null, not a crash") {
    val m = matcher("\\{")
    assertEquals(m.matchAt("{", 1), null)
  }

  test("fastAscii: a non-ASCII start character always falls through to the general walk") {
    val m = matcher("é", "éé") // "é" and "éé"
    assertEquals(m.matchAt("éx", 0), (priority = 0, end = 1))
    assertEquals(m.matchAt("éé", 0), (priority = 1, end = 2))
  }

  test("fastAscii: an ASCII literal that never matches at all still returns null") {
    val m = matcher("\\{")
    assertEquals(m.matchAt("x", 0), null)
  }
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
    assertEquals(identifierFirst.matchAt("42", 0), (priority = 0, end = 2))

    // moo.compile({ number: /[0-9]+/, identifier: /[a-z0-9]+/ }).reset('42').next()
    //   -> { type: 'number', value: '42' }
    val numberFirst = matcher("[0-9]+", "[a-z0-9]+")
    assertEquals(numberFirst.matchAt("42", 0), (priority = 0, end = 2))
  }

  test("moo/kotlinc: keyword ties with an identifier prefix, but a longer identifier wins (test.js, Kotlin.flex)") {
    // moo's motivating example for keywords-as-a-subset-of-identifier (test.js, README.md):
    // naive `{keyword: ['class'], identifier: /[a-zA-Z]+/}` still needs `class` to win on a
    // tie, and `identifier` to win once the match is strictly longer.
    val m = matcher("class", "[a-zA-Z]+")
    assertEquals(m.matchAt("class", 0), (priority = 0, end = 5))
    assertEquals(m.matchAt("className", 0), (priority = 1, end = 9))
  }

  test("jflex: a longer literal that fails partway through contributes no match at all (performance.md)") {
    // JFlex's own docs illustrate maximal munch with `averylongkeyword` vs `.` against input
    // "averylongjoke": the keyword shares a 9-char prefix ("averylong") with the input before
    // diverging at 'k' vs 'j'. A literal pattern only accepts at its exact full length, so
    // that dead-end prefix contributes nothing - only `.` (1 char) matches.
    val m = matcher("averylongkeyword", ".")
    assertEquals(m.matchAt("averylongjoke", 0), (priority = 1, end = 1))
  }

  // ---------------------------------------------------------------------------------------
  // Second pass: broader sweep of moo/JFlex/Kotlin, not just headline corner cases. Lengths
  // below were mechanically verified against the real upstream patterns (Node regex exec for
  // moo, direct regex reasoning cross-checked against the actual .flex sources for JFlex/
  // Kotlin) before translating pattern syntax to what this parser accepts (e.g. no lazy
  // quantifiers, no `[^]` "any char" - see inline notes on each case that needed translation).
  // ---------------------------------------------------------------------------------------

  test("moo: number rule's own two alternatives compete (test/python.js NUMBER rule, partial)") {
    val m = matcher("[0-9]+\\.[0-9]+", "[0-9]+")
    assertEquals(m.matchAt("12.04 rest", 0), (priority = 0, end = 5))
    assertEquals(m.matchAt("123 rest", 0), (priority = 1, end = 3))
    assertEquals(m.matchAt("3.14 rest", 0), (priority = 0, end = 4))
  }

  test("moo: triple-quote string beats single-quote alternative when applicable (test/python.js STRING rule)") {
    // Original moo patterns are lazily-quantified (`*?`) - this parser has no lazy quantifiers,
    // but Brzozowski-derivative matching finds the longest *accepting* prefix regardless of
    // greedy/lazy notation, so the plain greedy translation accepts the identical language and
    // gives the identical answer here. `[^]` (JS "any char incl. newline") becomes an explicit
    // full code-point range, since `[\s\S]`-style shorthands-in-a-class aren't supported here.
    val m = matcher("\"\"\"[\\x{0}-\\x{10FFFF}]*\"\"\"", "\"[^\"]*\"")
    assertEquals(m.matchAt("\"\"\"abc\"\"\" rest", 0), (priority = 0, end = 9))
    assertEquals(m.matchAt("\"abc\" rest", 0), (priority = 1, end = 5))
  }

  test("moo: 4-way numeric literal competition incl. a genuine tie (test/python.js NUMBER rule)") {
    val m = matcher(
      "(?:[0-9]+(?:\\.[0-9]+)?e-?[0-9]+)",
      "(?:(?:0|[1-9][0-9]*)?\\.[0-9]+)",
      "(?:(?:0|[1-9][0-9]*)\\.[0-9]*)",
      "(?:0|[1-9][0-9]*)",
    )
    assertEquals(m.matchAt("123.456e-7 rest", 0), (priority = 0, end = 10))
    assertEquals(m.matchAt("123. rest", 0), (priority = 2, end = 4))
    assertEquals(m.matchAt(".456 rest", 0), (priority = 1, end = 4))
    // genuine tie: alternatives 1 and 2 both match "0.5" (length 3); lower index wins
    assertEquals(m.matchAt("0.5 rest", 0), (priority = 1, end = 3))
  }

  test("moo: 3-way operator maximal munch, no declaration-order conflict (test/python.js opPat)") {
    val m = matcher("\\*\\*=", "\\*\\*", "\\*")
    assertEquals(m.matchAt("**= rest", 0), (priority = 0, end = 3))
  }

  test(
    ("moo: cross-rule literals are NOT longest-match in moo itself (test.js L179-186, \"moo\" vs \"moomintroll\")" +
      " - TODO: not portable as a positive case; moo's own test asserts the SHORTER, earlier-declared" +
      " \"moo\" (3 chars) wins here, but TokenMatcher does true cross-rule longest match and picks the" +
      " longer pattern instead - documenting the divergence, not a bug").ignore,
  ) {
    val m = matcher("moo", "moomintroll")
    assertEquals(m.matchAt("moomintroll", 0), (priority = 1, end = 11))
  }

  test("jflex: longest match beats declaration order (docs/md/example.md, breaker vs break)") {
    val m = matcher("break", "[a-zA-Z_][a-zA-Z0-9_]*")
    assertEquals(m.matchAt("breaker", 0), (priority = 1, end = 7))
  }

  test("jflex: exact tie broken by declaration order (docs/md/example.md, break keyword vs identifier)") {
    val m = matcher("break", "[a-zA-Z_][a-zA-Z0-9_]*")
    assertEquals(m.matchAt("break", 0), (priority = 0, end = 5))
  }

  test("jflex: longest beats declaration order, `=` vs `==` (docs/md/example.md)") {
    val m = matcher("=", "==")
    assertEquals(m.matchAt("==", 0), (priority = 1, end = 2))
  }

  test("jflex: 4-way shift-operator prefix cascade (examples/cup-java java.flex)") {
    val m = matcher("<", "<=", "<<", "<<=")
    assertEquals(m.matchAt("<<= rest", 0), (priority = 3, end = 3))
  }

  test("jflex: integer-literal `L` suffix (examples/cup-java java.flex)") {
    val m = matcher("0|[1-9][0-9]*", "(?:0|[1-9][0-9]*)[lL]")
    assertEquals(m.matchAt("123L rest", 0), (priority = 1, end = 4))
  }

  test("jflex: 3-way float/double/double-with-suffix literal (examples/cup-java java.flex)") {
    val fLit = "(?:[0-9]+\\.[0-9]*|\\.[0-9]+|[0-9]+)"
    val exponent = "[eE][+-]?[0-9]+"
    val m = matcher(
      s"$fLit(?:$exponent)?[fF]",
      s"$fLit(?:$exponent)?",
      s"$fLit(?:$exponent)?[dD]",
    )
    assertEquals(m.matchAt("3.14f rest", 0), (priority = 0, end = 5))
    assertEquals(m.matchAt("3.14d rest", 0), (priority = 2, end = 5))
    assertEquals(m.matchAt("3.14 rest", 0), (priority = 1, end = 4))
  }

  test("kotlin: `as` vs `as?` vs identifier, 3-way (Kotlin.flex L312/L315/L324)") {
    val m = matcher("as", "[a-zA-Z_][a-zA-Z0-9_]*", "as\\?")
    assertEquals(m.matchAt("as? rest", 0), (priority = 2, end = 3))
    assertEquals(m.matchAt("asdf rest", 0), (priority = 1, end = 4))
  }

  test("kotlin: `::` vs `:` (Kotlin.flex)") {
    val m = matcher("::", ":")
    assertEquals(m.matchAt("::foo", 0), (priority = 0, end = 2))
  }

  test("kotlin: `->` vs `-` (Kotlin.flex)") {
    val m = matcher("->", "-")
    assertEquals(m.matchAt("->x", 0), (priority = 0, end = 2))
  }

  test("kotlin: `..<` vs `..` vs `.`, 3-way (Kotlin.flex)") {
    val m = matcher("\\.\\.<", "\\.\\.", "\\.")
    assertEquals(m.matchAt("..<x", 0), (priority = 0, end = 3))
  }

  test("kotlin: integer vs double literal, longer wins despite later declaration (Kotlin.flex)") {
    val intLit = "[0-9][_0-9]*[Uu]?[Ll]?"
    val doubleLit = "(?:[0-9][_0-9]*)?\\.[0-9][_0-9]*(?:[eE][+-]?[_0-9]*)?[Ff]?"
    val m = matcher(intLit, doubleLit)
    assertEquals(m.matchAt("123.456 rest", 0), (priority = 1, end = 7))
  }

  test("kotlin: leading-dot double literal vs bare dot operator (Kotlin.flex)") {
    val doubleLit = "(?:[0-9][_0-9]*)?\\.[0-9][_0-9]*(?:[eE][+-]?[_0-9]*)?[Ff]?"
    val m = matcher("\\.", doubleLit)
    assertEquals(m.matchAt(".5 rest", 0), (priority = 1, end = 2))
  }

  test("kotlin: raw match winner for the `!in` decoy pattern (Kotlin.flex L316/L322/L354)") {
    // Kotlin's real lexer applies yypushback(3) on the 4-char decoy so the *emitted* tokens end
    // up as "!" + the identifier - that pushback is lexer-action machinery TokenMatcher doesn't
    // have. This only checks the raw winner-and-length TokenMatcher itself would report.
    val m = matcher("!in[a-zA-Z0-9_]", "!in", "!")
    assertEquals(m.matchAt("!inside", 0), (priority = 0, end = 4))
    assertEquals(m.matchAt("!in rest", 0), (priority = 1, end = 3))
  }

  test("kotlin: raw match winner for the integer/range decoy pattern (Kotlin.flex L278-279)") {
    val m = matcher("[0-9][_0-9]*[Uu]?[Ll]?\\.\\.", "[0-9][_0-9]*[Uu]?[Ll]?")
    assertEquals(m.matchAt("1..10", 0), (priority = 0, end = 3))
  }

  // fromRegexesBounded / fromSubsetsBounded --------------------------------

  test("fromRegexesBounded agrees with fromRegexes once the cap is generous enough") {
    val bounded = TokenMatcher.fromRegexesBounded(10_000)(parse("if"), parse("[a-zA-Z_][a-zA-Z0-9_]*"))
    assert(bounded.isRight)
    assertEquals(bounded.toOption.get.matchAt("ifx", 0), matcher("if", "[a-zA-Z_][a-zA-Z0-9_]*").matchAt("ifx", 0))
  }

  test("fromRegexesBounded fails fast once the cap is too small to build even the start state") {
    assertEquals(TokenMatcher.fromRegexesBounded(0)(parse("[a-zA-Z_][a-zA-Z0-9_]*")), Left(StateSpaceLimitExceeded(0)))
  }

  // A 100-way alternation of distinct fixed-length tokens builds a sizeable prefix-trie of DFA
  // states (same idea as `manyStatesEmpty` in `SubsetBenchmark`) - enough states that a cap of 1
  // is exceeded well before `compile` finishes, but a generous cap still succeeds.
  private lazy val manyStatesPattern = (0 until 100).map(i => Regex.literal(f"token$i%03d")).reduce(_ | _)

  test("fromRegexesBounded stays well-behaved (no exponential blowup) on a wide alternation") {
    assertEquals(TokenMatcher.fromRegexesBounded(1)(manyStatesPattern), Left(StateSpaceLimitExceeded(1)))
    assert(TokenMatcher.fromRegexesBounded(10_000)(manyStatesPattern).isRight)
  }

  // Classic ReDoS-shaped input: `(a|aa)*b` would catastrophically backtrack in a backtracking
  // engine. This engine has no backtracking, but the derivative-based state space it explores
  // instead has no bound of its own without an explicit cap - this asserts building a DFA from
  // several such patterns, combined and negated, stays within a modest cap instead of blowing up.
  test("fromRegexesBounded stays well-behaved on classic ReDoS-shaped patterns combined/negated") {
    val redosLike = parse("(a|aa)*b")
    val combined = redosLike.concat(redosLike) | !redosLike
    assert(TokenMatcher.fromRegexesBounded(1_000)(redosLike, combined).isRight)
  }

  // Capturing/named groups (Regex.Group) - erased the same way Subset.of erases them (see that
  // method's doc comment), so a pattern using them matches identically to its non-capturing
  // equivalent; matchAt itself never returns capture spans (see TokenMatcher's own doc comment).
  test("patterns with capturing/named groups compile and match identically to the non-capturing equivalent") {
    val withGroups = matcher("(if)", "(?<id>[a-zA-Z_][a-zA-Z0-9_]*)")
    val withoutGroups = matcher("if", "[a-zA-Z_][a-zA-Z0-9_]*")
    assertEquals(withGroups.matchAt("ifx", 0), withoutGroups.matchAt("ifx", 0))
    assertEquals(withGroups.matchAt("if", 0), withoutGroups.matchAt("if", 0))
  }
