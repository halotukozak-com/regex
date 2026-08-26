package halotukozak.regex

import halotukozak.regex.Regex.*

class RegexParserTest extends munit.FunSuite:

  private def parse(pattern: String): Regex = RegexParser.parse(pattern) match
    case Right(r) => r
    case Left(err) => fail(s"expected successful parse of /$pattern/, got $err")

  private def assertInvalidSyntax(result: Either[RegexParseError, Regex]): Unit = result match
    case Left(_: RegexParseError.InvalidSyntax) => ()
    case other => fail(s"expected InvalidSyntax, got $other")

  private def assertUnsupported(result: Either[RegexParseError, Regex]): Unit = result match
    case Left(_: RegexParseError.UnsupportedFeature) => ()
    case other => fail(s"expected UnsupportedFeature, got $other")

  test("RegexParseError's toString includes the message, position, and pattern") {
    val err: RegexParseError = RegexParser.parse("(?<=foo)").left.getOrElse(fail("expected a parse error"))
    val text = err.toString
    assert(text.contains("unsupported regex feature `lookbehind`"), text)
    assert(text.contains("at position 3"), text)
    assert(text.contains("\"(?<=foo)\""), text)
  }

  test("parses literal string") {
    assertEquals(parse("abc"), Regex.literal("abc"))
  }

  test("parses single literal") {
    assertEquals(parse("a"), Regex.lit('a'))
  }

  test("parses dot") {
    assertEquals(parse("."), Regex(CharSet.dotDefault))
  }

  test("parses character class with range") {
    assertEquals(parse("[a-z]"), Regex.range('a', 'z'))
  }

  test("parses negated character class") {
    assertEquals(parse("[^a-z]"), Regex(CharSet.range('a', 'z').complement))
  }

  test("parses character class with multiple ranges") {
    assertEquals(
      parse("[a-zA-Z0-9_]"),
      Regex(
        CharSet.normalize(
          Range('a', 'z'),
          Range('A', 'Z'),
          Range('0', '9'),
          Range('_', '_'),
        ),
      ),
    )
  }

  test("parses Kleene star") {
    assertEquals(parse("a*"), Regex.lit('a').star)
  }

  test("parses plus quantifier") {
    val a = Regex.lit('a')
    assertEquals(parse("a+"), a.concat(a.star))
  }

  test("parses optional quantifier") {
    assertEquals(parse("a?"), Eps | Regex.lit('a'))
  }

  test("parses bounded repetition {2}") {
    val a = Regex.lit('a')
    assertEquals(parse("a{2}"), a.concat(a))
  }

  test("parses unbounded repetition {2,}") {
    val a = Regex.lit('a')
    assertEquals(parse("a{2,}"), a.concat(a).concat(a.star))
  }

  test("parses alternation") {
    assertEquals(parse("a|b"), Regex.lit('a') | Regex.lit('b'))
  }

  test("parses group") {
    assertEquals(parse("(ab)"), Regex.literal("ab"))
  }

  test("parses non-capturing group") {
    assertEquals(parse("(?:ab)"), Regex.literal("ab"))
  }

  test("parses escapes") {
    assertEquals(parse("\\."), Regex.lit('.'))
    assertEquals(parse("\\*"), Regex.lit('*'))
    assertEquals(parse("\\\\"), Regex.lit('\\'))
    assertEquals(parse("\\t"), Regex.lit('\t'))
    assertEquals(parse("\\n"), Regex.lit('\n'))
  }

  test("parses \\d shorthand") {
    assertEquals(parse("\\d"), Regex.range('0', '9'))
  }

  test("parses ^ as StartAnchor") {
    assertEquals(parse("^a"), Regex.StartAnchor.concat(Regex.lit('a')))
  }

  test("parses $/\\Z/\\z as an end-of-input assertion") {
    val endOfInput = Regex.lookahead(Regex(CharSet.all), positive = false)
    assertEquals(parse("a$"), parse("a\\Z"))
    assertEquals(parse("a$"), parse("a\\z"))
    assertEquals(parse("a$"), Regex.lit('a').concat(endOfInput))
  }

  test("parses \\A as StartAnchor") {
    assertEquals(parse("\\Aa"), parse("^a"))
  }

  test("rejects word-boundary and \\G anchors") {
    assertUnsupported(RegexParser.parse("\\ba"))
    assertUnsupported(RegexParser.parse("\\Ba"))
    assertUnsupported(RegexParser.parse("\\Ga"))
  }

  test("rejects \\A/\\Z inside a character class") {
    assertInvalidSyntax(RegexParser.parse("[\\A]"))
    assertInvalidSyntax(RegexParser.parse("[\\Z]"))
  }

  test("parses positive lookahead") {
    assertEquals(parse("(?=a)"), Regex.lookahead(Regex.lit('a'), positive = true))
  }

  test("parses negative lookahead") {
    assertEquals(parse("(?!a)"), Regex.lookahead(Regex.lit('a'), positive = false))
  }

  test("lookahead body follows the normal group grammar") {
    assertEquals(parse("(?=a|b)"), Regex.lookahead(Regex.lit('a') | Regex.lit('b'), positive = true))
  }

  test("lookahead composes with what follows it") {
    val a = Regex.lit('a')
    assertEquals(parse("(?=a)a"), Regex.lookahead(a, positive = true).concat(a))
  }

  test("rejects lookbehind") {
    assertUnsupported(RegexParser.parse("(?<=a)"))
  }

  test("rejects backreferences") {
    assertUnsupported(RegexParser.parse("(a)\\1"))
  }

  test("rejects empty character class") {
    assertInvalidSyntax(RegexParser.parse("[]"))
  }

  test("rejects unclosed group") {
    assertInvalidSyntax(RegexParser.parse("(abc"))
  }

  test("parses nested groups") {
    assertEquals(parse("(a(bc))"), Regex.literal("abc"))
  }

  test("parses alternation inside groups") {
    assertEquals(parse("(a|b)"), Regex.lit('a') | Regex.lit('b'))
  }

  test("parses quantified group") {
    val ab = Regex.literal("ab")
    assertEquals(parse("(ab)+"), ab.concat(ab.star))
  }

  test("parses bounded {n,m} repetition") {
    assertEquals(parse("a{2,3}"), Regex.lit('a').repeat(2, 3))
  }

  test("parses character class with single char") {
    assertEquals(parse("[a]"), Regex.lit('a'))
  }

  test("parses character class with mixed escapes") {
    assertEquals(
      parse("[\\t\\n ]"),
      Regex(
        CharSet.normalize(
          Range('\t', '\t'),
          Range('\n', '\n'),
          Range(' ', ' '),
        ),
      ),
    )
  }

  test("unions a shorthand escape into a character class") {
    assertEquals(parse("[\\d]"), Regex(CharSet.range('0', '9')))
    assertEquals(
      parse("[\\da-f]"),
      Regex(CharSet.range('0', '9').union(CharSet.normalize(Range('a', 'f')))),
    )
  }

  test("unions a negated shorthand escape into a character class") {
    assertEquals(parse("[\\D]"), Regex(CharSet.range('0', '9').complement))
  }

  test("negates the union of a mixed character class") {
    assertEquals(parse("[^\\da-f]"), Regex(CharSet.range('0', '9').union(CharSet.normalize(Range('a', 'f'))).complement))
  }

  test("a shorthand escape can't start a range, but a following `-` is a literal member") {
    assertEquals(
      parse("[\\d-z]"),
      Regex(CharSet.range('0', '9').union(CharSet.normalize(Range('-', '-'), Range('z', 'z')))),
    )
  }

  test("rejects a shorthand escape ending a range") {
    assertInvalidSyntax(RegexParser.parse("[a-\\d]"))
  }

  test("computes in-class && intersection") {
    assertEquals(parse("[a-z&&[def]]"), Regex(CharSet.normalize(Range('d', 'f'))))
  }

  test("computes in-class && subtraction via negated nested subclass") {
    assertEquals(
      parse("[a-z&&[^bc]]"),
      Regex(CharSet.range('a', 'z').intersect(CharSet.normalize(Range('b', 'c')).complement)),
    )
  }

  test("chains multiple && intersections") {
    assertEquals(parse("[a-z&&[^m-p]&&[^a-c]]"), parse("[d-lq-z]"))
  }

  test("unions a nested subclass alongside plain members") {
    assertEquals(parse("[0-9[a-f]]"), Regex(CharSet.normalize(Range('0', '9'), Range('a', 'f'))))
  }

  test("a lone `&` is a literal member, not an intersection operator") {
    assertEquals(parse("[a&b]"), Regex(CharSet.normalize(Range('&', '&'), Range('a', 'a'), Range('b', 'b'))))
  }

  test("rejects an empty && operand") {
    assertInvalidSyntax(RegexParser.parse("[a-z&&]"))
  }

  test("parses \\s and \\w shorthands") {
    assertEquals(
      parse("\\s"),
      Regex(
        CharSet.normalize(
          Range(' ', ' '),
          Range('\t', '\t'),
          Range('\n', '\n'),
          Range(0x0b, 0x0b),
          Range('\f', '\f'),
          Range('\r', '\r'),
        ),
      ),
    )
    assertEquals(
      parse("\\w"),
      Regex(
        CharSet.normalize(
          Range('a', 'z'),
          Range('A', 'Z'),
          Range('0', '9'),
          Range('_', '_'),
        ),
      ),
    )
  }

  test("rejects unclosed char class") {
    assertInvalidSyntax(RegexParser.parse("[abc"))
  }

  test("rejects unclosed quantifier") {
    assertInvalidSyntax(RegexParser.parse("a{2"))
  }

  test("rejects dangling backslash") {
    assertInvalidSyntax(RegexParser.parse("a\\"))
  }

  test("rejects trailing input after pattern") {
    assertInvalidSyntax(RegexParser.parse("*"))
  }

  test("parses empty pattern as Eps") {
    assertEquals(parse(""), Eps)
  }

  test("parses \\xhh hex escape") {
    assertEquals(parse("\\x41"), Regex.lit('A'))
  }

  test("parses \\x{h...h} hex escape") {
    assertEquals(parse("\\x{41}"), Regex.lit('A'))
    assertEquals(parse("\\x{1F600}"), Regex(CharSet.single(0x1f600)))
  }

  test("rejects \\x{} escape above the max code point") {
    assertInvalidSyntax(RegexParser.parse("\\x{110000}"))
  }

  test("rejects incomplete \\x escape") {
    assertInvalidSyntax(RegexParser.parse("\\x4"))
    assertInvalidSyntax(RegexParser.parse("\\x4g"))
  }

  test("parses \\uhhhh unicode escape") {
    assertEquals(parse("\\u0041"), Regex.lit('A'))
  }

  test("parses \\0 octal escape") {
    assertEquals(parse("\\01"), Regex.lit(0x01.toChar))
    assertEquals(parse("\\012"), Regex.lit(0x0a.toChar))
    assertEquals(parse("\\0101"), Regex.lit('A'))
  }

  test("rejects \\0 with no following octal digit") {
    assertInvalidSyntax(RegexParser.parse("\\0"))
  }

  test("\\0 octal escape stops at 2 digits when a 3rd would overflow 0377") {
    // digits after the leading `0` are "777": 0o77 (63) would overflow to 0o777 (511 > 0o377),
    // so only the first two digits are consumed and the trailing `7` is a separate literal
    assertEquals(parse("\\0777"), Regex.lit(0x3f.toChar).concat(Regex.lit('7')))
  }

  test("parses \\cX control escape") {
    assertEquals(parse("\\cA"), Regex.lit(0x01.toChar))
  }

  test("rejects incomplete \\c escape") {
    assertInvalidSyntax(RegexParser.parse("\\c"))
  }

  test("parses \\Q...\\E quoted literal") {
    assertEquals(parse("\\Qa.b*\\E"), Regex.literal("a.b*"))
  }

  test("backslash has no special meaning inside \\Q...\\E") {
    assertEquals(parse("\\Qa\\b\\E"), Regex.literal("a\\b"))
  }

  test("parses unterminated \\Q as literal to end of pattern") {
    assertEquals(parse("\\Qa.b"), Regex.literal("a.b"))
  }

  test("parses hex/unicode/octal/control escapes inside a character class") {
    assertEquals(
      parse("[\\x41\\u0042\\0103\\cA]"),
      Regex(
        CharSet.normalize(
          Range('A', 'A'),
          Range('B', 'B'),
          Range('C', 'C'),
          Range(0x01, 0x01),
        ),
      ),
    )
  }

  test("parses \\R linebreak matcher") {
    val linebreak = Regex(
      CharSet.normalize(
        Range('\n', '\n'),
        Range(0x0b, 0x0b),
        Range('\f', '\f'),
        Range('\r', '\r'),
        Range(0x85, 0x85),
        Range(0x2028, 0x2029),
      ),
    )
    assertEquals(parse("\\R"), Regex.literal("\r\n") | linebreak)
  }

  test("\\b inside a character class means backspace") {
    assertEquals(parse("[\\b]"), Regex.lit(0x08.toChar))
  }

  test("\\b outside a character class is an unsupported word boundary") {
    assertUnsupported(RegexParser.parse("\\b"))
  }

  test("rejects undefined letter escapes") {
    assertInvalidSyntax(RegexParser.parse("\\m"))
    assertInvalidSyntax(RegexParser.parse("\\y"))
  }

  test("rejects unsupported but recognized Java escapes") {
    assertUnsupported(RegexParser.parse("\\G"))
    assertUnsupported(RegexParser.parse("\\k"))
    assertUnsupported(RegexParser.parse("\\X"))
  }

  test("accepts quantifier bounds up to the cap") {
    RegexParser.parse(s"a{${Regex.maxRepeatBound}}") match
      case Right(_) => ()
      case Left(err) => fail(s"expected successful parse, got $err")
  }

  test("rejects quantifier bounds above the cap") {
    assertInvalidSyntax(RegexParser.parse(s"a{${Regex.maxRepeatBound + 1}}"))
    assertInvalidSyntax(RegexParser.parse(s"a{0,${Regex.maxRepeatBound + 1}}"))
  }

  test("parses complex realistic pattern") {
    val idStart = Regex(
      CharSet.normalize(
        Range('a', 'z'),
        Range('A', 'Z'),
        Range('_', '_'),
      ),
    )
    val idRest = Regex(
      CharSet.normalize(
        Range('a', 'z'),
        Range('A', 'Z'),
        Range('0', '9'),
        Range('_', '_'),
      ),
    )
    assertEquals(parse("[a-zA-Z_][a-zA-Z0-9_]*"), idStart.concat(idRest.star))
  }
