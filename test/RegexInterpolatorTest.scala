package halotukozak.regex

import halotukozak.regex.Regex.*

import scala.compiletime.testing.{typeCheckErrors, typeChecks}

class RegexInterpolatorTest extends munit.FunSuite:

  // Cross-checks: the interpolator's compile-time expansion (RegexParser.parse followed by a
  // ToExpr[Regex] round-trip) must produce the exact same value as calling the parser directly.

  test("literal string") {
    assertEquals(regex"abc", RegexParser.parse("abc").toOption.get)
    assertEquals(regex"abc", Regex.literal("abc"))
  }

  test("dot") {
    assertEquals(regex".", RegexParser.parse(".").toOption.get)
  }

  test("character class with range") {
    assertEquals(regex"[a-z]", RegexParser.parse("[a-z]").toOption.get)
  }

  test("negated character class") {
    assertEquals(regex"[^a-z]", RegexParser.parse("[^a-z]").toOption.get)
  }

  test("alternation") {
    assertEquals(regex"a|b", RegexParser.parse("a|b").toOption.get)
  }

  test("Kleene star, plus, and optional") {
    assertEquals(regex"a*", RegexParser.parse("a*").toOption.get)
    assertEquals(regex"a+", RegexParser.parse("a+").toOption.get)
    assertEquals(regex"a?", RegexParser.parse("a?").toOption.get)
  }

  test("bounded repetition") {
    assertEquals(regex"a{2,3}", RegexParser.parse("a{2,3}").toOption.get)
  }

  // Regression for a real compiler crash (not just a slow compile). Two layers fixed this:
  // `Regex.Repeat` keeps `{lo,hi}` symbolic instead of unfolding it into that many `Concat`/`Alt`
  // nodes, and `ToExpr[Regex]` embeds any tree as flat `Array[Int]` tables (`Regex.fromEncoded`)
  // instead of one nested constructor call per node. Either alone left a real crash: unfolding
  // alone still recursed too deep encoding/splicing it, and flat-splicing alone doesn't help if
  // `.repeat` still built a 1000-node tree to begin with. `regex"a{0,1000}"` reliably
  // `StackOverflowError`'d the compiler before both landed - first inside `ToExpr[Regex]`'s own
  // recursion, then, once that was made iterative, inside a *later* compiler phase (`Splicer`)
  // walking the still-deep generated tree.
  test("maximally deep quantifier compiles and round-trips without overflowing the compiler") {
    assertEquals(regex"a{1000,1000}", RegexParser.parse("a{1000,1000}").toOption.get)
    assertEquals(regex"a{0,1000}", RegexParser.parse("a{0,1000}").toOption.get)
  }

  // Regression: a long literal string builds one Concat node per character (Regex.literal
  // folds right), independent of any {lo,hi} quantifier - so it stresses the same compiler-
  // stack-safety concern from a different angle. 2000 chars comfortably exercises this without
  // nearing the JVM's unrelated ~64KB-bytecode-per-method ceiling on the flat Array[Int] literals
  // ToExpr[Regex] embeds - a hard classfile limit no macro-embedding strategy can lift, and not
  // one this fix claims to.
  test("long literal string compiles without overflowing the compiler") {
    val long =
      regex"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    assertEquals(
      long,
      Regex.literal("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
    )
  }

  test("groups") {
    assertEquals(regex"(a(bc))+", RegexParser.parse("(a(bc))+").toOption.get)
  }

  test("lookahead") {
    assertEquals(regex"(?=a)a", RegexParser.parse("(?=a)a").toOption.get)
    assertEquals(regex"(?!a)b", RegexParser.parse("(?!a)b").toOption.get)
  }

  // IntelliJ's Scala plugin (pre-2025.2) misapplies `s`-interpolator escape-cooking rules to
  // custom interpolators taking an `args: Any*` parameter, flagging `\d`/`\Q`/`\x` below as
  // "Invalid escape character" — a known false positive (fixed in the 2025.2 release; update
  // the plugin if you still see it). Scala 3 itself leaves non-builtin interpolator literals
  // uncooked/raw (verified: `scala-cli test` compiles and passes these), so `\d` etc. reach
  // RegexParser byte-for-byte, same as a plain "\\d" string.

  test("shorthand escapes \\d \\s \\w") {
    assertEquals(regex"\d", RegexParser.parse("\\d").toOption.get)
    assertEquals(regex"\s", RegexParser.parse("\\s").toOption.get)
    assertEquals(regex"\w", RegexParser.parse("\\w").toOption.get)
  }

  test("\\Q...\\E quoted literal") {
    assertEquals(regex"\Qa.b*c\E", RegexParser.parse("\\Qa.b*c\\E").toOption.get)
  }

  test("\\x and \\u code point escapes") {
    assertEquals(regex"\x41", RegexParser.parse("\\x41").toOption.get)
    assertEquals(regex"A", RegexParser.parse("\\u0041").toOption.get)
  }

  // Compile-time validation: a pattern that's a literal at the call site is parsed and
  // type-checked while compiling the interpolator call, not deferred to runtime.

  test("valid literal pattern type-checks") {
    assert(typeChecks("""_root_.scala.StringContext("a(b|c)*").regex()"""))
  }

  test("unsupported feature fails to compile with a descriptive message") {
    val errors = typeCheckErrors("""_root_.scala.StringContext("(?<=foo)").regex()""")
    assertEquals(errors.size, 1)
    assert(
      errors.head.message.contains("Regex parse error") &&
        errors.head.message.contains("unsupported regex feature `lookbehind`") &&
        errors.head.message.contains("at position 3") && errors.head.message.contains(""""(?<=foo)""""),
      s"unexpected message: ${errors.head.message}",
    )
  }

  test("invalid syntax fails to compile with a descriptive message") {
    val errors = typeCheckErrors("""_root_.scala.StringContext("(").regex()""")
    assertEquals(errors.size, 1)
    assert(
      errors.head.message.contains("Regex parse error") && errors.head.message.contains("at position 1") &&
        errors.head.message.contains(""""(""""),
      s"unexpected message: ${errors.head.message}",
    )
  }

  // Runtime fallback: when the StringContext isn't a compile-time constant (e.g. built up
  // dynamically rather than written as a `regex"..."` literal), parsing happens at runtime
  // instead, mirroring RegexParser.parse's Either result via a thrown exception on failure.

  private def nonConstantParts(parts: String*): Seq[String] =
    if System.nanoTime() != 0 then parts else Seq.empty

  test("non-constant StringContext parses successfully at runtime") {
    val sc = StringContext(nonConstantParts("a", "b")*)
    assertEquals(sc.regex(), RegexParser.parse("ab").toOption.get)
  }

  test("non-constant StringContext throws on an invalid pattern at runtime") {
    val sc = StringContext(nonConstantParts("(")*)
    intercept[IllegalArgumentException](sc.regex())
  }

  // Current limitation: `args` exists only so `regex"..."` type-checks as a string
  // interpolator; the values substituted into `${...}` holes are not incorporated into the
  // pattern, only the literal parts are. This documents that behavior rather than endorsing it.

  test("interpolated splice values are not incorporated into the pattern") {
    val ignored = "this text never appears in the resulting pattern"
    assertEquals(regex"a${ignored}b", Regex.literal("ab"))
  }
