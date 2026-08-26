package halotukozak.regex

import scala.compiletime.testing.{typeCheckErrors, typeChecks}

class TokenMatcherMacroTest extends munit.FunSuite:

  // Cross-check: the macro's compile-time expansion (RegexParser.parse per pattern, then
  // TokenMatcher.compile, then a ToExpr[TokenMatcher] round-trip) must behave identically to
  // building the same matcher from the runtime API.

  test("tokenMatcher builds a matcher equivalent to fromRegexes, at compile time") {
    val compileTime = tokenMatcher("if", "[a-zA-Z_][a-zA-Z0-9_]*")
    val runtime = TokenMatcher.fromRegexes(
      RegexParser.parse("if").toOption.get,
      RegexParser.parse("[a-zA-Z_][a-zA-Z0-9_]*").toOption.get,
    )
    assertEquals(compileTime.matchAt("ifx", 0), runtime.matchAt("ifx", 0))
    assertEquals(compileTime.matchAt("ifx", 0), Some((priority = 1, end = 3)))
    assertEquals(compileTime.matchAt("if", 0), Some((priority = 0, end = 2)))
  }

  test("tokenMatcher with a single pattern") {
    val m = tokenMatcher("[0-9]+")
    assertEquals(m.matchAt("123abc", 0), Some((priority = 0, end = 3)))
    assertEquals(m.matchAt("abc", 0), None)
  }

  test("tokenMatcher ties broken by lowest priority index, same as fromRegexes") {
    val m = tokenMatcher("break", "[a-zA-Z_][a-zA-Z0-9_]*")
    assertEquals(m.matchAt("break", 0), Some((priority = 0, end = 5)))
    assertEquals(m.matchAt("breaker", 0), Some((priority = 1, end = 7)))
  }

  // Compile-time validation: every pattern is parsed and compiled into a DFA while compiling
  // the tokenMatcher(...) call itself, not deferred to runtime.

  test("a literal pattern list type-checks") {
    assert(typeChecks("""_root_.halotukozak.regex.tokenMatcher("if", "[a-zA-Z_][a-zA-Z0-9_]*")"""))
  }

  test("a non-literal pattern fails to compile with a descriptive message") {
    val errors =
      typeCheckErrors("""{ val notLiteral = "a"; _root_.halotukozak.regex.tokenMatcher(notLiteral, "b") }""")
    assertEquals(errors.size, 1)
    assert(
      errors.head.message.contains("tokenMatcher patterns must be string literals known at compile time"),
      s"unexpected message: ${errors.head.message}",
    )
  }

  test("an unsupported/invalid pattern fails to compile with a descriptive message") {
    val errors = typeCheckErrors("""_root_.halotukozak.regex.tokenMatcher("(unterminated", "b")""")
    assertEquals(errors.size, 1)
    assert(
      errors.head.message.contains("Regex parse error") && errors.head.message.contains("at position 13"),
      s"unexpected message: ${errors.head.message}",
    )
  }

  test("a spread (non-vararg-literal) pattern sequence fails to compile with a descriptive message") {
    val errors =
      typeCheckErrors("""{ val patterns = Seq("a", "b"); _root_.halotukozak.regex.tokenMatcher(patterns*) }""")
    assertEquals(errors.size, 1)
    assert(
      errors.head.message.contains("tokenMatcher requires a literal, statically-known list of pattern strings"),
      s"unexpected message: ${errors.head.message}",
    )
  }
