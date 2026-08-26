/**
 * Regenerates `regex/UnicodeCategories.scala` from the host JDK's `java.lang.Character.getType`.
 *
 * Run: `scala-cli run scripts/GenUnicodeCategories.scala > regex/UnicodeCategories.scala`
 *
 * Deliberately lives outside `regex/`, excluded from the library's own build the same way the
 * `bench` directory is (see the `--exclude` patterns for both directories in
 * `.github/workflows/ci.yml`) - this script calls `java.lang.Character` directly, a JVM-only
 * API the library's own sources may never depend on (see the generated file's own doc comment
 * for why). Only this script's *output* is checked in; the script itself never runs as part of
 * building, testing, or publishing the library.
 *
 * Re-run this whenever the checked-in `UnicodeCategories.scala` needs auditing, or regenerating
 * against a different JDK's bundled Unicode version - it always reproduces that file byte for
 * byte from the running JDK's own `Character.getType` classification, so there's never a
 * question of whether the checked-in ranges still match what generated them (previously they
 * didn't have a reproducible source at all - this script replaces a one-off, discarded version
 * of itself that produced the ranges currently checked in).
 */
@main def genUnicodeCategories(): Unit =
  // Category codes assigned by `java.lang.Character`'s `getType`, keyed by the two-letter
  // Unicode General_Category alias `java.util.regex.Pattern` itself uses for `\p{Lu}` etc.
  // `Character.UNASSIGNED` ("Cn") is deliberately excluded - see the generated file's own doc
  // comment for why.
  val typeNames = Map(
    Character.UPPERCASE_LETTER -> "Lu",
    Character.LOWERCASE_LETTER -> "Ll",
    Character.TITLECASE_LETTER -> "Lt",
    Character.MODIFIER_LETTER -> "Lm",
    Character.OTHER_LETTER -> "Lo",
    Character.NON_SPACING_MARK -> "Mn",
    Character.COMBINING_SPACING_MARK -> "Mc",
    Character.ENCLOSING_MARK -> "Me",
    Character.DECIMAL_DIGIT_NUMBER -> "Nd",
    Character.LETTER_NUMBER -> "Nl",
    Character.OTHER_NUMBER -> "No",
    Character.CONNECTOR_PUNCTUATION -> "Pc",
    Character.DASH_PUNCTUATION -> "Pd",
    Character.START_PUNCTUATION -> "Ps",
    Character.END_PUNCTUATION -> "Pe",
    Character.INITIAL_QUOTE_PUNCTUATION -> "Pi",
    Character.FINAL_QUOTE_PUNCTUATION -> "Pf",
    Character.OTHER_PUNCTUATION -> "Po",
    Character.MATH_SYMBOL -> "Sm",
    Character.CURRENCY_SYMBOL -> "Sc",
    Character.MODIFIER_SYMBOL -> "Sk",
    Character.OTHER_SYMBOL -> "So",
    Character.SPACE_SEPARATOR -> "Zs",
    Character.LINE_SEPARATOR -> "Zl",
    Character.PARAGRAPH_SEPARATOR -> "Zp",
    Character.CONTROL -> "Cc",
    Character.FORMAT -> "Cf",
    Character.PRIVATE_USE -> "Co",
    Character.SURROGATE -> "Cs",
  )

  // One entry per two-letter category: its ranges, merged as contiguous code points are found
  // while scanning 0..CharSet.maxCodePoint (0x10FFFF) in order.
  val ranges = scala.collection.mutable.Map.empty[String, scala.collection.mutable.ArrayBuffer[(Int, Int)]]
  typeNames.values.foreach(name => ranges(name) = scala.collection.mutable.ArrayBuffer.empty)

  var cp = 0
  while cp <= 0x10ffff do
    typeNames.get(Character.getType(cp).toByte).foreach { name =>
      val buf = ranges(name)
      if buf.nonEmpty && buf.last._2 == cp - 1 then buf(buf.length - 1) = (buf.last._1, cp)
      else buf += ((cp, cp))
    }
    cp += 1

  def encode(name: String): String = ranges(name).iterator.flatMap((lo, hi) => Iterator(lo, hi)).mkString(",")

  val twoLetterEntries = typeNames.values.toList.sorted
    .map(name => s"""    "$name" -> decode("${encode(name)}"),""")
    .mkString("\n")

  // Built by concatenation, not written literally, so this generator's own source never
  // contains a real doc-comment open/close token pair around the *emitted* doc comments below -
  // scala-cli's using-directives pre-scan reads comment delimiters textually (it runs before
  // real tokenization, to find using-directive lines cheaply), and gets confused into
  // misreading this file's own comment boundaries if those delimiters appear a second time
  // inside a string literal.
  val co = "/*" + "*"
  val cc = "*" + "/"
  def doc(line: String): String = s"$co $line $cc"

  println(
    s"""package halotukozak.regex
       |
       |$co
       | * Unicode general-category range tables backing `\\p{...}`/`\\P{...}` in [[RegexParser]].
       | *
       | * The ranges below were generated once, offline, from `java.lang.Character.getType` (the host
       | * JDK's bundled Unicode version) - a one-off codegen step, not a runtime dependency: this file
       | * has no `java.lang.Character`/`java.util.regex` call anywhere in it, so the resulting data is
       | * exactly as portable to Scala.js/Scala Native as [[CharSet.dotDefault]] or the `\\d`/`\\s`/`\\w`
       | * shorthand sets already are - all of them are just `Range`s baked in ahead of time.
       | *
       | * Each category is encoded as one comma-joined `lo,hi,lo,hi,...` string, not an
       | * `Array[Range]`/`Array[(Int,Int)]` literal, for the same reason [[Regex.RegexEncoder]] embeds
       | * its node table as a string instead of one array element per node: `Lu`/`Ll` alone are 600+
       | * ranges, and that many array-literal elements would each cost their own
       | * `dup`/`ldc`/`iastore` triplet, while a single string constant is one `ldc` regardless of length.
       | *
       | * `Cn` (Unassigned) is deliberately omitted: unlike the other 29 categories - stable,
       | * positively-defined partitions of the code space - `Cn` means "everything not otherwise
       | * assigned", so its membership grows with every Unicode revision and would silently drift with
       | * whatever Unicode version the host JDK happens to bundle.
       | *
       | * General categories only - script (`\\p{IsGreek}`) and block (`\\p{InGreek}`) properties aren't
       | * supported (out of scope for 1.0.0), the same kind of deliberate restriction this parser
       | * already makes keeping `(?i)` ASCII-only rather than pulling in the rest of Java's
       | * Unicode-aware regex machinery.
       | *
       | * Generated by `scripts/GenUnicodeCategories.scala` - re-run that script to regenerate this
       | * file (e.g. against a newer JDK's Unicode version) rather than hand-editing the tables below.
       | $cc
       |private[regex] object UnicodeCategories:
       |
       |  private def decode(encoded: String): CharSet =
       |    val ints = encoded.split(',').map(_.toInt)
       |    CharSet.normalize((0 until ints.length / 2).map(i => Range(ints(2 * i), ints(2 * i + 1))))
       |
       |  ${doc("""Two-letter Unicode general categories, e.g. `"Lu"` (uppercase letter), `"Nd"` (decimal digit).""")}
       |  private val twoLetter: Map[String, CharSet] = Map(
       |$twoLetterEntries
       |  )
       |
       |  ${doc("One-letter aggregate categories, each the union of its constituent two-letter categories.")}
       |  private val oneLetter: Map[String, CharSet] = Map(
       |    "L" -> Set("Lu", "Ll", "Lt", "Lm", "Lo").map(twoLetter).reduce(_ | _),
       |    "M" -> Set("Mn", "Mc", "Me").map(twoLetter).reduce(_ | _),
       |    "N" -> Set("Nd", "Nl", "No").map(twoLetter).reduce(_ | _),
       |    "P" -> Set("Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po").map(twoLetter).reduce(_ | _),
       |    "S" -> Set("Sm", "Sc", "Sk", "So").map(twoLetter).reduce(_ | _),
       |    "Z" -> Set("Zs", "Zl", "Zp").map(twoLetter).reduce(_ | _),
       |    "C" -> Set("Cc", "Cf", "Co", "Cs").map(twoLetter).reduce(_ | _),
       |  )
       |
       |  ${doc("""Looks up a Unicode general-category name (`"L"`, `"Lu"`, `"Nd"`, ...), if recognized.""")}
       |  def get(name: String): Option[CharSet] = oneLetter.get(name).orElse(twoLetter.get(name))
       |""".stripMargin,
  )
