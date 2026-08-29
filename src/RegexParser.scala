package halotukozak.regex

import halotukozak.regex.Regex.Eps

import scala.annotation.{switch, tailrec}
import scala.collection.mutable

/** Reason a [[RegexParser.parse]] call did not produce a [[Regex]]. */
sealed trait RegexParseError:
  def pattern: String
  def position: Int
  def message: String

  /**
   * Overridden here (rather than left to each case class) so it's picked up by every case
   * class's synthesized `toString` slot instead of the default field dump they'd otherwise
   * generate (`InvalidSyntax((abc,3,expected \`)\` at position 3)`) — callers that just
   * interpolate the error value directly get a readable message "for free".
   */
  override def toString: String = s"""$message (at position $position in "$pattern")"""

object RegexParseError:

  /** Pattern is syntactically malformed (unterminated group, dangling backslash, etc.). */
  final case class InvalidSyntax(pattern: String, position: Int, message: String) extends RegexParseError

  /** Pattern uses a recognized but unsupported feature (anchors, lookaround, backreferences, ...). */
  final case class UnsupportedFeature(pattern: String, position: Int, feature: String) extends RegexParseError:
    def message: String = s"unsupported regex feature `$feature`"

/**
 * Java-regex-style parser producing a normalized [[Regex]].
 *
 * Supported subset (see [[Regex]] doc): literals, escapes (\d \D \s \S \w \W \t \n \r \f
 * \a \e \v \cX \0[n[n]] \xhh \x{h...h} \uhhhh \Q...\E \R and meta-escapes \. \* \+ \? \(
 * \) \[ \] \{ \} \| \^ \$ \-), Unicode property escapes \p{...} \P{...} (general categories
 * like `L`/`Lu`/`Nd`, and the ASCII-only POSIX classes like `Alpha`/`Digit` - see
 * [[UnicodeCategories]]; script/block properties are unsupported), `.`, char classes `[...]`
 * `[^...]` with ranges, nested shorthand escapes (`[\da-f]`), nested subclasses, and `&&`
 * intersection (`[a-z&&[^aeiou]]`) (`\b` inside a class means backspace, matching Java),
 * alternation `|`, capturing groups `(...)`, named groups `(?<name>...)`, non-capturing groups
 * `(?:...)`, lookahead `(?=...)` `(?!...)`, the `i` inline flag `(?i)` `(?-i)` `(?i:...)`
 * `(?-i:...)` (ASCII-only case folding, matching Java's `CASE_INSENSITIVE` without
 * `UNICODE_CASE`), quantifiers `*` `+` `?` `{n}` `{n,}` `{n,m}` (bounds capped at
 * [[Regex.maxRepeatBound]]), anchors `^` `$` `\A` `\Z` `\z`.
 *
 * Capturing/named groups are recognized structurally (numbered/named correctly - see
 * [[Regex.Group]]) but their captured spans aren't extractable yet - that's tracked
 * separately, same as [[Regex]]'s own doc comment notes.
 *
 * Unsupported: word-boundary anchors `\b` `\B`, `\G`, lookbehind, backreferences
 * `\1`..`\9` `\k<name>` `\g{...}`, Unicode script/block properties (`\p{IsGreek}`,
 * `\p{InGreek}`, etc.), grapheme clusters `\X`, other inline flags (`(?m)`, `(?s)`, `(?d)`,
 * `(?u)`, `(?x)`, `(?U)`). Any other undefined letter escape (e.g. `\m`, `\y`, `\q`) is
 * rejected as invalid syntax, matching `java.util.regex.Pattern`'s own behavior.
 */
object RegexParser:

  private final class InvalidSyntaxSignal(val msg: String, val pos: Int) extends RuntimeException(msg)
  private final class UnsupportedSignal(val feature: String, val pos: Int) extends RuntimeException(feature)

  /** What kind of group `(...)` a header (`?:`, `?=`, `?!`, `?i`, `?<name>` &c., or none) introduced. */
  private enum GroupKind:
    /** `(?:...)`: grouping/precedence only, no capture - the only non-capturing kind. */
    case Plain

    /** A bare `(...)`: capturing, numbered `index` (assigned left-to-right by opening paren). */
    case Capturing(index: Int)

    /** `(?<name>...)`: capturing, both by `index` and by `name` - see [[Regex.Group]]. */
    case NamedCapturing(index: Int, name: String)

    case Look(positive: Boolean)

    /** `(?i)`/`(?-i)`: mutates the *enclosing* scope's flag state, not a scope of its own. */
    case FlagDirective(setI: Boolean, clearI: Boolean)

    /** `(?i:...)`/`(?-i:...)`: flag applies only within `...`, restored once it closes. */
    case ScopedFlags(setI: Boolean, clearI: Boolean)

  private val whitespaceSet: CharSet = CharSet.normalize(
    Range(' ', ' '),
    Range('\t', '\t'),
    Range('\n', '\n'),
    Range(0x0b, 0x0b),
    Range('\f', '\f'),
    Range('\r', '\r'),
  )

  private val wordSet: CharSet = CharSet.normalize(
    Range('a', 'z'),
    Range('A', 'Z'),
    Range('0', '9'),
    Range('_', '_'),
  )

  /**
   * ASCII-only POSIX classes for `\p{Lower}` etc., matching `java.util.regex.Pattern`'s default
   * (non-`UNICODE_CHARACTER_CLASS`) behavior: unlike general-category names (`\p{L}`, `\p{Nd}`,
   * ...; see [[UnicodeCategories]]), which are always true Unicode, these predefined POSIX names
   * are always plain ASCII regardless of any Unicode-awareness flag - the same ASCII-only
   * restriction this parser already makes for `(?i)`.
   */
  private val posixClasses: Map[String, CharSet] = Map(
    "Lower" -> CharSet.range('a', 'z'),
    "Upper" -> CharSet.range('A', 'Z'),
    "ASCII" -> CharSet.range(0, 0x7f),
    "Alpha" -> CharSet.normalize(Range('a', 'z'), Range('A', 'Z')),
    "Digit" -> CharSet.range('0', '9'),
    "Alnum" -> CharSet.normalize(Range('a', 'z'), Range('A', 'Z'), Range('0', '9')),
    // `[\p{Alnum}\p{Punct}]` (Graph, below) covers exactly 0x21-0x7e, so listing the individual
    // punctuation characters (as the Java javadoc's own definition does) would just describe the
    // same range through 32 separate one-character `Range`s.
    "Punct" -> (
      CharSet.range(0x21, 0x2f) | CharSet.range(0x3a, 0x40) | CharSet.range(0x5b, 0x60) | CharSet.range(0x7b, 0x7e)
    ),
    "Graph" -> CharSet.range(0x21, 0x7e),
    "Print" -> CharSet.range(0x20, 0x7e),
    "Blank" -> CharSet.normalize(Range(' ', ' '), Range('\t', '\t')),
    "Cntrl" -> CharSet.normalize(Range(0x00, 0x1f), Range(0x7f, 0x7f)),
    "XDigit" -> CharSet.normalize(Range('0', '9'), Range('a', 'f'), Range('A', 'F')),
    "Space" -> whitespaceSet,
  )

  /** What `\R` matches as a single code point, i.e. everything but the two-char `\r\n` case. */
  private val linebreakSet: CharSet = CharSet.normalize(
    Range('\n', '\n'),
    Range(0x0b, 0x0b),
    Range('\f', '\f'),
    Range('\r', '\r'),
    Range(0x85, 0x85),
    Range(0x2028, 0x2029),
  )

  /**
   * `$` / `\Z` / `\z`. Under this library's whole-string `matches()` semantics (never `find`),
   * all three coincide exactly: none of them can themselves consume the trailing line
   * terminator Java's `$`/`\Z` are normally lenient about — that leniency only matters for
   * `find`/`lookingAt`-style matching, where trailing input is allowed to go unconsumed. Here
   * "assert nothing remains" is precisely "no single code point (incl. line terminators) can
   * follow", i.e. `(?!.)` with `.` meaning *any* code point, not the `dotDefault` used for a
   * literal `.` atom.
   */
  private val endOfInput: Regex = Regex.lookahead(Regex(CharSet.all), positive = false)

  /**
   * Parse `pattern` into a [[Regex]]. Returns [[Left]] with structured error info if the
   * pattern is malformed or uses an unsupported feature.
   */
  def parse(pattern: String): Either[RegexParseError, Regex] =
    try
      val p = new Parser(pattern)
      val r = p.parseAlt()
      if p.pos != pattern.length then
        Left(RegexParseError.InvalidSyntax(pattern, p.pos, s"unexpected trailing input at position ${p.pos}"))
      else Right(r)
    catch
      case e: InvalidSyntaxSignal => Left(RegexParseError.InvalidSyntax(pattern, e.pos, e.msg))
      case e: UnsupportedSignal => Left(RegexParseError.UnsupportedFeature(pattern, e.pos, e.feature))

  private final class Parser(private val src: String):
    var pos: Int = 0

    /**
     * `(?i)` state, threaded through parsing rather than the `Regex`/`Subset` algebra - Java's
     * default `CASE_INSENSITIVE` (without `UNICODE_CASE`) is plain ASCII a-z/A-Z folding, so
     * every literal/range this parser produces while this is `true` gets expanded to include
     * its counterpart at the point it's built, per [[literalCharSet]]/[[foldCharSet]]. Scoped
     * like an ordinary variable would be in a recursive-descent parser: saved and restored
     * around a group's body in [[parseGroup]], except for a bare `(?i)` directive, which has no
     * body of its own and instead mutates whatever scope encloses it - matching Java, where
     * `(a(?i)b)c` doesn't apply case-insensitivity to the `c` outside the group.
     */
    private var caseInsensitive: Boolean = false

    /** Next 1-based capturing-group number to hand out - see [[GroupKind.Capturing]]. */
    private var nextGroupIndex: Int = 1

    /**
     * Names already claimed by an earlier `(?<name>...)` in this pattern - Java requires
     * every named group's name to be unique within the pattern.
     */
    private val usedGroupNames = mutable.Set.empty[String]

    private def nextIndex(): Int =
      val i = nextGroupIndex
      nextGroupIndex += 1
      i

    private def fail(msg: String): Nothing =
      throw new InvalidSyntaxSignal(msg, pos)

    private def unsupported(feature: String): Nothing =
      throw new UnsupportedSignal(feature, pos)

    private def eof: Boolean = pos >= src.length

    private def cur: Char = src.charAt(pos)

    private def consume(): Char =
      val c = src.charAt(pos)
      pos += 1
      c

    private def expect(c: Char): Unit =
      if eof || cur != c then fail(s"expected `$c` at position $pos")
      pos += 1

    /**
     * ASCII-only case fold of a single range: itself, plus (if it overlaps `A-Z`/`a-z`) the
     * corresponding shifted counterpart sub-range - matching `java.util.regex`'s
     * `CASE_INSENSITIVE` default (no `UNICODE_CASE`): only plain ASCII letters fold, nothing
     * else (verified against `java.util.regex`: e.g. `(?i)straße` does not match
     * "STRASSE"). Handles partial overlaps correctly (e.g. folding `[Y-b]`, which spans `Y Z [
     * \ ] ^ _ \` a b`, adds `y z` and `A B` for the letter-only sub-ranges without touching the
     * symbols in between).
     */
    private def foldRange(lo: Int, hi: Int): CharSet =
      val base = CharSet.range(lo, hi)
      val upperLo = math.max(lo, 'A'.toInt)
      val upperHi = math.min(hi, 'Z'.toInt)
      val withUpper = if upperLo <= upperHi then base | CharSet.range(upperLo + 32, upperHi + 32) else base
      val lowerLo = math.max(lo, 'a'.toInt)
      val lowerHi = math.min(hi, 'z'.toInt)
      if lowerLo <= lowerHi then withUpper | CharSet.range(lowerLo - 32, lowerHi - 32) else withUpper

    /**
     * Folds every range in `set` independently, then reunions them. Applying this to a
     * character class's content *before* negating it (see `parseClassBody`) is what makes
     * `(?i)[^a-z]` correctly exclude `A-Z` too, rather than only excluding lowercase - verified
     * against `java.util.regex`. Also correctly handles `&&` (`(?i)[a-z&&[^aeiou]]`): each
     * bracketed operand independently folds-then-negates via its own `parseClassBody` call
     * before the two are intersected, so the vowel exclusion already covers both cases by the
     * time the outer intersection (and this fold) runs.
     */
    private def foldCharSet(set: CharSet): CharSet =
      set.iterator.foldLeft(CharSet.empty)((acc, r) => acc | foldRange(r.lo, r.hi))

    /** A single literal code point, case-folded if `(?i)` is currently active. */
    private def literalCharSet(c: Int): CharSet = if caseInsensitive then foldRange(c, c) else CharSet.single(c)

    /** Like [[Regex.literal]], but folds each character per [[literalCharSet]] along the way. */
    private def literalText(text: String): Regex =
      text.foldRight(Eps: Regex)((c, acc) => Regex(literalCharSet(c.toInt)).concat(acc))

    /** alt = concat ('|' concat)* */
    def parseAlt(): Regex =
      @tailrec def loop(acc: Vector[Regex]): Vector[Regex] =
        if !eof && cur == '|' then
          pos += 1
          loop(acc :+ parseConcat())
        else acc
      val parts = loop(Vector(parseConcat()))
      if parts.sizeIs == 1 then parts.head else Regex.alt(parts)

    /** concat = factor* */
    private def parseConcat(): Regex =
      @tailrec def loop(acc: Vector[Regex]): Vector[Regex] =
        if eof then acc
        else
          (cur: @switch) match
            case ')' | '|' => acc
            case _ => loop(acc :+ parseFactor())
      val parts = loop(Vector.empty)
      if parts.isEmpty then Eps else parts.reduceRight(_ concat _)

    /** factor = atom quantifier? */
    private def parseFactor(): Regex =
      val a = parseAtom()
      if eof then a
      else
        (cur: @switch) match
          case '*' =>
            pos += 1
            a.star
          case '+' =>
            pos += 1
            a.concat(a.star)
          case '?' =>
            pos += 1
            // `a` first, not `Eps | a`: `Regex.alt`/`|` preserve the order they're given (see
            // Regex.Alt's doc) precisely so a capture-aware consumer can tell "prefer entering
            // `a`" (this quantifier's greedy default) from "prefer skipping it" - order that was
            // unobservable before any such consumer existed, but is now load-bearing.
            a | Eps
          case '{' =>
            pos += 1
            parseRepeat(a)
          case _ => a

    private def parseRepeat(a: Regex): Regex =
      val lo = readNumber()
      if lo < 0 then fail(s"expected number after `{` at position $pos")
      if eof then fail(s"unterminated `{` at position $pos")
      val hi = (cur: @switch) match
        case ',' =>
          pos += 1
          if !eof && cur == '}' then Int.MaxValue
          else
            val n = readNumber()
            if n < 0 then fail(s"expected number or `}` after `,` at position $pos")
            n
        case '}' => lo
        case _ => fail(s"expected `,` or `}` at position $pos")
      expect('}')
      if hi < lo then fail(s"invalid quantifier bounds {$lo,$hi}: upper bound must be >= lower bound")
      if lo > Regex.maxRepeatBound || (hi != Int.MaxValue && hi > Regex.maxRepeatBound) then
        fail(s"quantifier bound exceeds maximum supported value of ${Regex.maxRepeatBound} (got {$lo,$hi})")
      a.repeat(lo, hi)

    private def readNumber(): Int =
      @tailrec def loop(p: Int): Int = if p < src.length && src.charAt(p).isDigit then loop(p + 1) else p

      val end = loop(pos)
      if end == pos then -1
      else
        val text = src.substring(pos, end)
        pos = end
        try text.toInt
        catch case _: NumberFormatException => fail(s"quantifier value `$text` does not fit in an Int")

    /** atom = group | charClass | `.` | escape | char */
    private def parseAtom(): Regex =
      if eof then fail("unexpected end of pattern")
      (cur: @switch) match
        case '(' => parseGroup()
        case '[' => parseCharClass()
        case '.' =>
          pos += 1
          Regex(CharSet.dotDefault)
        case '\\' => parseEscape()
        case '^' =>
          pos += 1
          Regex.StartAnchor
        case '$' =>
          pos += 1
          endOfInput
        case ')' | '|' | '*' | '+' | '?' | '{' | '}' | ']' =>
          fail(s"unexpected `$cur` at position $pos")
        case c =>
          pos += 1
          Regex(literalCharSet(c.toInt))

    private def parseGroup(): Regex =
      expect('(')
      val kind =
        if !eof && cur == '?' then
          pos += 1
          consumeGroupHeader()
        else GroupKind.Capturing(nextIndex())
      kind match
        // No save/restore here: unlike every other group kind, a bare `(?i)`/`(?-i)` isn't a
        // scope of its own - its body is always empty (`)` follows immediately) - it mutates
        // whatever scope encloses it, same as an ordinary assignment would.
        case GroupKind.FlagDirective(setI, clearI) =>
          applyFlags(setI, clearI)
          val inner = parseAlt()
          expect(')')
          inner
        case GroupKind.Plain =>
          scoped:
            val inner = parseAlt()
            expect(')')
            inner
        case GroupKind.Capturing(index) =>
          scoped:
            val inner = parseAlt()
            expect(')')
            Regex.group(index, None, inner)
        case GroupKind.NamedCapturing(index, name) =>
          scoped:
            val inner = parseAlt()
            expect(')')
            Regex.group(index, Some(name), inner)
        case GroupKind.Look(positive) =>
          scoped:
            val inner = parseAlt()
            expect(')')
            Regex.lookahead(inner, positive)
        case GroupKind.ScopedFlags(setI, clearI) =>
          scoped:
            applyFlags(setI, clearI)
            val inner = parseAlt()
            expect(')')
            inner

    /** Runs `body`, restoring `caseInsensitive` to its pre-`body` value afterward. */
    private def scoped(body: => Regex): Regex =
      val saved = caseInsensitive
      val result = body
      caseInsensitive = saved
      result

    private def applyFlags(setI: Boolean, clearI: Boolean): Unit =
      if setI then caseInsensitive = true
      if clearI then caseInsensitive = false

    private def consumeGroupHeader(): GroupKind =
      if eof then unsupported("incomplete group header")
      (cur: @switch) match
        case ':' =>
          pos += 1
          GroupKind.Plain
        case '=' =>
          pos += 1
          GroupKind.Look(positive = true)
        case '!' =>
          pos += 1
          GroupKind.Look(positive = false)
        case '<' =>
          pos += 1
          if !eof && (cur == '=' || cur == '!') then unsupported("lookbehind")
          else
            val name = parseGroupName()
            GroupKind.NamedCapturing(nextIndex(), name)
        case _ => parseFlagGroupHeader()

    /**
     * `name>` following `(?<` (the `<` itself already consumed by the caller). Java requires
     * `[a-zA-Z][a-zA-Z0-9]*`, unique within the whole pattern - both violations are
     * `InvalidSyntax` (matching `java.util.regex.Pattern`'s own `PatternSyntaxException` for
     * both), not `UnsupportedFeature`: the `(?<name>...)` form itself is fully supported, this
     * specific name just isn't well-formed or available.
     */
    private def parseGroupName(): String =
      val start = pos
      while !eof && cur != '>' do pos += 1
      if eof then fail(s"unterminated named group starting at position $start")
      val name = src.substring(start, pos)
      pos += 1
      if name.isEmpty || !name.charAt(0).isLetter || !name.tail.forall(_.isLetterOrDigit) then
        fail(s"""invalid group name "$name" at position $start""")
      if !usedGroupNames.add(name) then fail(s"named capturing group <$name> is already defined")
      name

    /**
     * `flags`, `flags-flags`, `:` or `)` following either - only `i` is a recognized flag
     * letter here; any other letter from Java's `idmsuxU` set is a recognized-but-unsupported
     * feature (`(?m)`, `(?s)`, ... aren't implemented yet), and anything else is a malformed
     * group header.
     */
    private def parseFlagGroupHeader(): GroupKind =
      val setI = consumeIFlag()
      val clearI = if !eof && cur == '-' then
        pos += 1; consumeIFlag()
      else false
      if eof then unsupported("incomplete group header")
      (cur: @switch) match
        case ':' =>
          pos += 1
          GroupKind.ScopedFlags(setI, clearI)
        case ')' => GroupKind.FlagDirective(setI, clearI)
        case _ => unsupported("flag group")

    private def consumeIFlag(): Boolean =
      if eof then false
      else if cur == 'i' then
        pos += 1
        true
      else if "dmsuxU".contains(cur) then unsupported(s"flag group `(?${consume()})`")
      else false

    private def parseCharClass(): Regex = Regex(parseClassBody())

    /** True at a `&&` intersection operator; a lone `&` is just a literal member. */
    private def atIntersectionOp: Boolean = !eof && cur == '&' && pos + 1 < src.length && src.charAt(pos + 1) == '&'

    /**
     * `[...]`, from the opening `[` through its matching `]` — used both for the top-level
     * class atom and, recursively, for `[nested]` subclasses on either side of `&&`
     * (`[a-z&&[^bc]]`). Each side of `&&` is independently negatable.
     */
    private def parseClassBody(): CharSet =
      expect('[')
      val negated = !eof && cur == '^'
      if negated then pos += 1
      val set = parseClassIntersection()
      expect(']')
      val folded = if caseInsensitive then foldCharSet(set) else set
      if negated then folded.complement else folded

    /** intersection = union ('&&' union)* */
    private def parseClassIntersection(): CharSet =
      @tailrec def loop(acc: CharSet): CharSet =
        if atIntersectionOp then
          pos += 2
          loop(acc.intersect(parseClassUnion()))
        else acc
      loop(parseClassUnion())

    /**
     * union = (range | shorthandEscape | `[nested]`)*, stopping at `]` or `&&`. A shorthand
     * class item (`\d`, `\s`, `\w`, ...) contributes to `shorthand` directly and can't start or
     * end a `-` range, matching Java: `[\d-z]` is digit, '-', and 'z' as three separate members
     * (the dash just isn't treated as a range operator there), while `[a-\d]` is a syntax error
     * (no single code point to range up to). Likewise a `-` immediately before `&&` is a
     * literal trailing dash rather than the start of a range.
     */
    private def parseClassUnion(): CharSet =
      @tailrec def loop(ranges: Vector[Range], extra: CharSet): (Vector[Range], CharSet) =
        if !eof && cur != ']' && !atIntersectionOp then
          if cur == '[' then loop(ranges, extra.union(parseClassBody()))
          else
            readClassChar() match
              case Left(set) => loop(ranges, extra.union(set))
              case Right(lo) =>
                val hi =
                  if !eof && cur == '-' && pos + 1 < src.length && src.charAt(pos + 1) != ']' && !atIntersectionOpAt(
                      pos + 1,
                    )
                  then
                    pos += 1
                    readClassChar() match
                      case Left(_) => fail(s"invalid character-class range: shorthand escape can't end a range")
                      case Right(hi) => hi
                  else lo
                if hi < lo then fail(s"invalid character-class range `${lo.toChar}-${hi.toChar}`: end must be >= start")
                loop(ranges :+ Range(lo, hi), extra)
        else (ranges, extra)
      val (ranges, extra) = loop(Vector.empty, CharSet.empty)
      if ranges.isEmpty && extra.isEmpty then fail("empty character class")
      // Skips the union (and the second normalizing pass it implies) in the common case where
      // this operand has no shorthand escape or nested subclass at all.
      if extra.isEmpty then CharSet.normalize(ranges) else CharSet.normalize(ranges).union(extra)

    private def atIntersectionOpAt(p: Int): Boolean = p < src.length && src.charAt(p) == '&' && p + 1 < src.length &&
      src.charAt(p + 1) == '&'

    private def readClassChar(): Either[CharSet, Int] =
      if eof then fail("unterminated character class")
      (cur: @switch) match
        case '\\' =>
          pos += 1
          readEscapedChar(inClass = true)
        case _ => Right(consume().toInt)

    private def parseEscape(): Regex =
      expect('\\')
      if eof then fail("dangling backslash")
      (cur: @switch) match
        case 'Q' =>
          pos += 1
          parseQuoted()
        case 'R' =>
          pos += 1
          Regex.literal("\r\n") | Regex(linebreakSet)
        case 'A' =>
          pos += 1
          Regex.StartAnchor
        case 'Z' | 'z' =>
          pos += 1
          endOfInput
        case _ =>
          readEscapedChar(inClass = false) match
            case Left(set) => Regex(set)
            case Right(c) => Regex(literalCharSet(c))

    /** Consumes literal text up to (and including) `\E`, or to the end of the pattern if absent. */
    private def parseQuoted(): Regex =
      val end = src.indexOf("\\E", pos)
      val text = if end < 0 then src.substring(pos) else src.substring(pos, end)
      pos = if end < 0 then src.length else end + 2
      literalText(text)

    /**
     * Reads char following `\\`. Either expands to a [[CharSet]] (shorthand) or yields a
     * single code point. `inClass` mirrors Java's rule that `\b` means backspace inside a
     * character class, rather than a word boundary.
     */
    private def readEscapedChar(inClass: Boolean): Either[CharSet, Int] =
      if eof then fail("dangling backslash")
      val c = src.charAt(pos)
      pos += 1
      (c: @switch) match
        case 'd' => Left(CharSet.range('0', '9'))
        case 'D' => Left(CharSet.range('0', '9').complement)
        case 's' => Left(whitespaceSet)
        case 'S' => Left(whitespaceSet.complement)
        case 'w' => Left(wordSet)
        case 'W' => Left(wordSet.complement)
        case 't' => Right('\t'.toInt)
        case 'n' => Right('\n'.toInt)
        case 'r' => Right('\r'.toInt)
        case 'f' => Right('\f'.toInt)
        case 'a' => Right(0x07)
        case 'e' => Right(0x1b)
        case 'v' => Right(0x0b)
        case 'b' => if inClass then Right(0x08) else unsupported(s"word boundary `\\$c`")
        case 'B' => unsupported(s"word boundary `\\$c`")
        // `\A`/`\Z`/`\z` are handled in `parseEscape` before reaching here, so this is only
        // ever hit for the in-class path (`[\A]` etc.) - Java rejects those as invalid syntax
        // (not merely unsupported), which the fallthrough to the `isLetter` case below matches.
        case 'G' => unsupported(s"anchor `\\$c`")
        case 'p' | 'P' => Left(parseUnicodeProperty(negated = c == 'P'))
        case 'k' => unsupported("named backreference `\\k`")
        case 'g' => unsupported("backreference `\\g`")
        case 'X' => unsupported("grapheme cluster `\\X`")
        case 'c' => Right(readControlEscape())
        case 'x' => Right(readHexEscape())
        case 'u' => Right(readUnicodeEscape())
        case '0' => Right(readOctalEscape())
        case _ =>
          if c.isDigit then unsupported(s"backreference `\\$c`")
          else if c.isLetter then fail(s"illegal/unsupported escape sequence `\\$c` at position $pos")
          else Right(c.toInt)

    /**
     * `\p{Name}` / `\P{Name}` (negated): a Unicode general category (`L`, `Lu`, `Nd`, ...; see
     * [[UnicodeCategories]]) or an ASCII-only POSIX class (`Lower`, `Alpha`, ..., in
     * `posixClasses` above). Script/block properties (`IsGreek`, `InGreek`, ...) and other named
     * binary properties aren't recognized - `unsupported`, not `fail`, since the `\p{...}`
     * syntax itself is well-formed, just naming a property this parser doesn't implement.
     */
    private def parseUnicodeProperty(negated: Boolean): CharSet =
      if eof || cur != '{' then fail(s"expected `{` after `\\${if negated then 'P' else 'p'}` at position $pos")
      pos += 1
      val start = pos
      while !eof && cur != '}' do pos += 1
      if eof then fail("unterminated `\\p{...}` escape")
      val name = src.substring(start, pos)
      pos += 1
      val set = UnicodeCategories
        .get(name)
        .orElse(posixClasses.get(name))
        .getOrElse(unsupported(s"Unicode property `\\${if negated then 'P' else 'p'}{$name}`"))
      if negated then set.complement else set

    /** `\cx`: control character `x XOR 0x40`. */
    private def readControlEscape(): Int =
      if eof then fail(s"incomplete `\\c` escape at position $pos")
      consume().toInt ^ 0x40

    /** `\xhh` (exactly 2 hex digits) or `\x{h...h}` (1+ hex digits, a valid code point). */
    private def readHexEscape(): Int =
      if !eof && cur == '{' then
        pos += 1
        val start = pos
        while !eof && cur != '}' do pos += 1
        if eof then fail("unterminated `\\x{...}` escape")
        val text = src.substring(start, pos)
        pos += 1
        if text.isEmpty then fail("empty `\\x{}` escape")
        val cp = text.toHexIntOpt.getOrElse(fail(s"invalid hexadecimal escape sequence `\\x{$text}`"))
        if cp < 0 || cp > CharSet.maxCodePoint then fail(s"invalid code point `\\x{$text}`")
        cp
      else
        if pos + 2 > src.length then fail("incomplete `\\x` escape")
        val text = src.substring(pos, pos + 2)
        val v = text.toHexIntOpt.getOrElse(fail(s"invalid hexadecimal escape sequence `\\x$text`"))
        pos += 2
        v

    /** `\uhhhh`: exactly 4 hex digits. */
    private def readUnicodeEscape(): Int =
      if pos + 4 > src.length then fail("incomplete `\\u` escape")
      val text = src.substring(pos, pos + 4)
      val v = text.toHexIntOpt.getOrElse(fail(s"invalid unicode escape sequence `\\u$text`"))
      pos += 4
      v

    /** `\0n`, `\0nn` or `\0mnn` octal escape, mirroring `java.util.regex.Pattern`'s own grammar. */
    private def readOctalEscape(): Int =
      def isOctal(ch: Char): Boolean = ch >= '0' && ch <= '7'
      if eof || !isOctal(cur) then fail("illegal octal escape sequence")
      val n = consume() - '0'
      if !eof && isOctal(cur) then
        val m = consume() - '0'
        if !eof && isOctal(cur) && (n << 6) + (m << 3) + (cur - '0') <= 0xff then
          val o = consume() - '0'
          n * 64 + m * 8 + o
        else n * 8 + m
      else n

extension (str: String)
  private def toHexIntOpt: Option[Int] =
    try Some(Integer.parseInt(str, 16))
    catch case _: NumberFormatException => None
