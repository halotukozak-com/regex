package halotukozak.regex

/**
 * Conformance cases adapted from other regex engines' own test suites, to cross-check this
 * parser's syntax/escape/algebra semantics against real-world references. Only the interesting
 * pattern/expectation data is adapted here (translated to this library's API) — not the
 * original test code itself.
 *
 * Cases exercising features this engine intentionally doesn't support (POSIX/Unicode property
 * classes, lookaround, backreferences, named groups, in-class `&&` intersection, or a
 * Matcher-style find/replace/split API this engine never had) are `ignore`d with a `TODO`
 * rather than deleted, so the gap stays visible instead of silently disappearing.
 */
class RegexConformanceTest extends munit.FunSuite:

  private def parse(pattern: String): Regex = RegexParser.parse(pattern) match
    case Right(r) => r
    case Left(err) => fail(s"expected successful parse of /$pattern/, got $err")

  private def subsetOf(pattern: String): Subset = Subset.parse(pattern) match
    case Right(s) => s
    case Left(err) => fail(s"expected successful parse of /$pattern/, got $err")

  private def assertInvalidSyntax(result: Either[RegexParseError, Regex]): Unit = result match
    case Left(_: RegexParseError.InvalidSyntax) => ()
    case other => fail(s"expected InvalidSyntax, got $other")

  private def assertUnsupported(result: Either[RegexParseError, Regex]): Unit = result match
    case Left(_: RegexParseError.UnsupportedFeature) => ()
    case other => fail(s"expected UnsupportedFeature, got $other")

  /**
   * Decodes a UTF-16 string into full Unicode code points, combining surrogate pairs.
   * Hand-rolled instead of `String#codePoints()` (a `java.util.stream` API not implemented
   * by Scala.js's javalib) to keep this cross-platform, matching the library under test.
   */
  private def codePointsOf(s: String): List[Int] =
    @scala.annotation.tailrec
    def loop(i: Int, acc: List[Int]): List[Int] =
      if i >= s.length then acc.reverse
      else
        val c1 = s.charAt(i)
        val isHighSurrogate = c1 >= 0xd800 && c1 <= 0xdbff
        if isHighSurrogate && i + 1 < s.length && s.charAt(i + 1) >= 0xdc00 && s.charAt(i + 1) <= 0xdfff then
          val c2 = s.charAt(i + 1)
          val cp = 0x10000 + (c1 - 0xd800) * 0x400 + (c2 - 0xdc00)
          loop(i + 2, cp :: acc)
        else loop(i + 1, c1.toInt :: acc)
    loop(0, Nil)

  /** Whole-string acceptance, i.e. `java.util.regex.Pattern.matches` semantics (not `find`). */
  private def matches(pattern: String, s: String): Boolean =
    codePointsOf(s).foldLeft(subsetOf(pattern))((acc, cp) => acc.derive(cp)).nullable

  private def isSubset(a: String, b: String): Boolean = subsetOf(a).subset(subsetOf(b))

  private def isProperSubset(a: String, b: String): Boolean = subsetOf(a).properSubset(subsetOf(b))

  private def doIntersect(a: String, b: String): Boolean = !Subset.of(parse(a) & parse(b)).isEmpty

  private def equiv(a: String, b: String): Boolean =
    val (sa, sb) = (subsetOf(a), subsetOf(b))
    sa.subset(sb) && sb.subset(sa)

  private def equivRegex(a: Regex, b: Regex): Boolean =
    val (sa, sb) = (Subset.of(a), Subset.of(b))
    sa.subset(sb) && sb.subset(sa)

  // ---------------------------------------------------------------------------------------
  // Adapted from OpenJDK's java.util.regex.RegExTest
  // https://github.com/openjdk/jdk/blob/master/test/jdk/java/util/regex/RegExTest.java
  // ---------------------------------------------------------------------------------------

  test("jdk: octal escapes (octalTest)") {
    assert(matches("\\u0007", ""))
    assert(matches("\\07", ""))
    assert(matches("\\007", ""))
    assert(matches("\\0007", ""))
    assert(matches("\\040", " "))
    assert(matches("\\0403", " 3"))
    assert(matches("\\0103", "C"))
  }

  test("jdk: basic hex/octal/unicode escapes agree (escapes)") {
    assert(matches("\\043", "#"))
    assert(matches("\\x23", "#"))
    assert(matches("\\u0023", "#"))
  }

  test("jdk: \\Q...\\E terminates on the first literal \\E; backslashes inside are not special (escapedSegmentTest)") {
    assertEquals(parse("\\Qdir1\\dir2\\E"), Regex.literal("dir1\\dir2"))
    assertEquals(parse("\\Qdir1\\dir2\\\\E"), Regex.literal("dir1\\dir2\\"))
  }

  test("jdk: \\x{h...h} addresses the full code point range, incl. supplementary plane (unicodeHexNotationTest)") {
    // no ^/$ anchors: this engine's `matches` is always whole-string, like the JDK original's
    // intent, but ^/$ themselves are unsupported anchor syntax here
    assert(matches("\\x{1033c}", "𐌼"))
  }

  test("jdk: \\xhh is a raw BMP code unit, not a UTF-8 byte decoder (unicodeHexNotationTest)") {
    // 4 separate \xHH escapes must NOT combine into the one supplementary code point they'd
    // decode to as UTF-8 bytes (0xF0 0x90 0x8C 0xBC -> U+1033C)
    assert(!matches("\\xF0\\x90\\x8C\\xBC", "𐌼"))
  }

  test("jdk: rejects malformed \\x{...} escapes (unicodeHexNotationTest)") {
    assertInvalidSyntax(RegexParser.parse("\\x{-23}"))
    assertInvalidSyntax(RegexParser.parse("\\x{}"))
    assertInvalidSyntax(RegexParser.parse("\\x{AB[ef]"))
  }

  test("jdk: rejects a bare trailing backslash (unescapedBackslash)") {
    assertInvalidSyntax(RegexParser.parse("\\"))
  }

  test("jdk: rejects a quantifier bound too large to fit in an Int (illegalRepetitionRange)") {
    assertInvalidSyntax(RegexParser.parse(".{4294967296}"))
  }

  test(
    ("jdk: in-class intersection `[a&&b]` (droppedClassesWithIntersection)" +
      " - TODO: not supported; `&&` is currently silently misparsed as literal chars instead of" +
      " being rejected or computing the intersection").ignore,
  ) {
    assertEquals(parse("[A-Z&&[A-Z]0-9]"), Regex.range('A', 'Z'))
  }

  test(
    ("jdk: POSIX/Unicode property classes \\p{Lower} etc. (unicodeClassesTest)" +
      " - TODO: unsupported, parser rejects \\p/\\P with UnsupportedFeature").ignore,
  ) {
    assertEquals(parse("\\p{Lower}"), Regex.range('a', 'z'))
  }

  // ---------------------------------------------------------------------------------------
  // Adapted from Kotlin's kotlin.text.Regex test suite
  // https://github.com/JetBrains/kotlin/blob/master/libraries/stdlib/test/text/RegexTest.kt
  //
  // Most of Kotlin's regex tests exercise a Matcher-style API this engine intentionally never
  // had (find/findAll/replace/split/capture groups/backreferences/lookaround/MULTILINE) - those
  // aren't ported since there's no equivalent surface to test here.
  // ---------------------------------------------------------------------------------------

  test("kotlin: escaping an arbitrary non-alphanumeric char (matchEscapeRandomChar)") {
    assert(matches("\\-", "-"))
  }

  test("kotlin: octal char value (matchCharWithOctalValue)") {
    assert(matches("a\\0141", "aa"))
  }

  test(
    ("kotlin: named group + \\k<name> backreference (matchNamedGroupsWithBackReference)" +
      " - TODO: unsupported, no capture groups or backreferences at all").ignore,
  ) {
    assert(matches("(?<title>\\w+), yes \\k<title>", "Sir, yes Sir"))
  }

  // ---------------------------------------------------------------------------------------
  // Adapted from dregex's own test suite (marianobarrios/dregex), the DFA-based algebra library
  // this project replaces for cross-platform use.
  // https://github.com/marianobarrios/dregex/blob/master/src/test/java/dregex/OperationsTest.java
  // https://github.com/marianobarrios/dregex/blob/master/src/test/java/dregex/EquivalenceTest.java
  // ---------------------------------------------------------------------------------------

  test("dregex: subset relation (testSubsetBoolean)") {
    assert(isSubset("a", "."))
    assert(isSubset("", ".*"))
    assert(isSubset("a", "a"))
    assert(isSubset("(a|b){2}", "[ab][ab]"))
    assert(!isSubset("[^a]", "[a]"))
    assert(!isSubset("[abc]", "[ab]"))
  }

  test("dregex: proper-subset relation (testProperSubsetBoolean)") {
    assert(isProperSubset("a", "."))
    assert(isProperSubset("", ".*"))
    assert(isProperSubset("[ab]+", "[ab]*"))
    assert(isProperSubset("[ab]", "[abcd]"))
    assert(!isProperSubset("a", "a"))
    assert(!isProperSubset("(a|b){2}", "[ab][ab]"))
  }

  test("dregex: intersection non-emptiness, no lookaround needed (testIntersectionsBoolean)") {
    assert(doIntersect("a", "."))
    assert(!doIntersect("a", "b"))
    assert(!doIntersect("[^a]", "a"))
    assert(!doIntersect("[^a]", "[a]"))
    assert(!doIntersect("[^ab]", "[ab]"))
    assert(!doIntersect("[^ab]", "a|b"))
  }

  test("dregex: union results (testUnion, lookaround-free subset)") {
    assert(equivRegex(parse("a") | parse("a"), parse("a")))
    assert(equivRegex(parse("a") | parse("b"), parse("a|b")))
    // dregex's `.` is compiled with Pattern.DOTALL (matches everything); ours matches Java's
    // DOTALL-off default (excludes line terminators), so compare against an explicit full range
    // instead of relying on `.`
    assert(equivRegex(parse("a") | parse("[^a]"), parse("[\\x{0}-\\x{10FFFF}]")))
  }

  test("dregex: intersection results (testIntersections, lookaround-free subset)") {
    assert(equivRegex(parse("a") & parse("."), parse("a")))
  }

  test("dregex: quantifier equivalences (testQuantifiers)") {
    assert(equiv("(a|b)+", "(a+|b+)+"))
    assert(equiv("a+", "aa*"))
    assert(equiv("a*a*", "a*"))
    assert(equiv("a?a*", "a*"))
    assert(equiv("(ab)+", "ab(ab)*"))
    assert(equiv("a", "a{1}"))
    assert(equiv("aa", "a{2}"))
    assert(equiv("aaa", "a{3}"))
    assert(equiv("a{0}", ""))
    assert(equiv("(a{2}){3}", "a{6}"))
    assert(equiv("(a{2}){3}", "a{5}a"))
    assert(!equiv("(a{2}){3}", "a{5}"))
    assert(equiv("a{2,3}", "aaa?"))
    assert(equiv("a{2,3}", "a{2}a?"))
    assert(equiv("a{0,3}", "a{0,2}a?"))
    assert(equiv("a{3,}", "a{3}a*"))
    assert(equiv("a{3,}", "a{2}a+"))
    assert(equiv("a{3,}", "aaa+"))
  }

  test("dregex: character class equivalences (testCharactedClasses)") {
    assert(equiv("[a]", "a"))
    assert(equiv("a|b|c", "[abc]"))
    assert(equiv("[abcdef]", "[a-f]"))
    assert(equiv("[a-cdef]", "[a-f]"))
    assert(equiv("[a-cd-f]", "[a-f]"))
  }

  test("dregex: shorthand character classes (testShortcutCharacterClasses)") {
    assert(equiv("\\d", "[0-9]"))
    assert(equiv("\\w", "[a-zA-Z0-9_]"))
    assert(equiv("\\s", "[ \\t\\n\\r\\f\\x{B}]"))
  }

  test("dregex: shorthand classes nested inside a character class, e.g. `[\\d]` (testShortcutCharacterClasses)") {
    assert(equiv("\\d", "[\\d]"))
  }

  test("dregex: \\Q...\\E block quotes (testBlockQuotesAndLiteralFlag)") {
    assert(equiv("\\Q\\E", ""))
    assert(equiv("\\Qabc\\E", "abc"))
    assert(equiv("\\Qa*\\E", "a\\*"))
    assert(equiv("(\\Qa*\\E)*", "(a\\*)*"))
    assert(equiv("\\Q(\\E", "\\("))
    assert(equiv("\\Q)\\E", "\\)"))
    assert(equiv("(\\Q)a\\E)", "\\)a"))
    assert(equiv("\\Q|\\E", "\\|"))
  }

  test("dregex: named group rejected as unsupported (testGrouping)") {
    assertUnsupported(RegexParser.parse("(?<name>abc)"))
  }

  test(
    ("dregex: lookaround equivalences (testLookaround)" +
      " - TODO: unsupported, lookaround is rejected at parse time").ignore,
  ) {
    assert(equiv("(?!a|b)(?!c).*", "(?!a|b|c).*"))
  }

  test(
    ("dregex: POSIX character classes (testPosixCharacterClasses)" +
      " - TODO: unsupported, \\p{...} is rejected at parse time").ignore,
  ) {
    assert(equiv("\\p{Lower}", "[a-z]"))
  }
