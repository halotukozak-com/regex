# regex

A pure-Scala symbolic regex algebra with exact language containment ("is this pattern a subset
of that one?") — not just string matching. Cross-built for the JVM, Scala.js, and Scala Native
from a single source tree, with zero dependency on `java.util.regex` or any other JVM-only API.

## Overview

`java.util.regex.Pattern`, JavaScript's `RegExp`, and every other backtracking regex engine let
you test whether a string matches a pattern — but none of them expose whether one pattern's
language is a subset of another's. Answering that requires an algebraic representation you can
run set operations on, not a black-box matcher.

`regex` builds that representation directly: an ADT of regular-expression combinators
(concatenation, alternation, intersection, complement, Kleene star, bounded repetition,
lookahead) with smart constructors that keep it normalized, plus a
[Brzozowski-derivative](https://en.wikipedia.org/wiki/Brzozowski_derivative)-based `Subset` view
exposing emptiness, nullability, and subset checks. A hand-written parser accepts a
Java-`Pattern`-compatible syntax subset (escapes, character classes, quantifiers, `\Q...\E`,
etc. — see the [[RegexParser]] scaladoc for exactly what's supported).

Typical use case: build-time or load-time validation that one input pattern doesn't shadow
another — e.g. catching lexer token rules where an earlier rule's language already contains a
later one's, so the later rule could never fire.

## Installation

Published to Maven Central under `com.halotukozak`. Requires Scala 3.

### scala-cli

```scala
//> using scala 3.9.0
//> using dep com.halotukozak::regex::0.2.0
```

### sbt

```scala
scalaVersion := "3.9.0"
libraryDependencies += "com.halotukozak" %% "regex" % "0.2.0"
```

### mill

```scala
def scalaVersion = "3.9.0"
def mvnDeps = Seq(mvn"com.halotukozak::regex::0.2.0")
```

## Quickstart

```scala
import halotukozak.regex.{RegexParser, Subset}

// Parse two patterns and ask whether the first's language is contained in the second's.
val idToken = Subset.parse("[a-zA-Z_][a-zA-Z0-9_]*").toOption.get
val keyword = Subset.parse("if").toOption.get

keyword.subset(idToken) // true - "if" would never be reached as its own token after idToken

// Or work with the underlying algebra directly.
import halotukozak.regex.Regex

val digits = Regex.range('0', '9')
val hexDigits = digits | Regex.range('a', 'f') | Regex.range('A', 'F')
```

`RegexParser.parse` returns `Either[RegexParseError, Regex]`, distinguishing malformed syntax
(`RegexParseError.InvalidSyntax`) from syntax this parser recognizes but doesn't support, like
lookbehind or backreferences (`RegexParseError.UnsupportedFeature`).

### `regex"..."` interpolator

For a pattern that's a string literal at the call site, `regex"..."` parses it at compile time
instead, so a malformed or unsupported pattern is a compile error rather than a `Left` you have
to remember to handle:

```scala
import halotukozak.regex.regex

val idToken = regex"[a-zA-Z_][a-zA-Z0-9_]*" // Regex, built while compiling

val bad = regex"(?<=foo)" // doesn't compile: Regex parse error: UnsupportedFeature(...)
```

If the pattern isn't a literal (e.g. it's assembled into a `StringContext` at runtime), it falls
back to parsing at runtime and throws `IllegalArgumentException` on failure instead. Note that
`${...}` splices aren't currently supported — only the literal parts of the string are used, so
`regex"a${x}b"` parses as `"ab"`, ignoring `x`.

### Capturing group spans

`CaptureMatcher` extracts the spans matched by `(...)`/`(?<name>...)` groups from a whole-string
match, backed by a no-backtracking NFA engine (see its own scaladoc for why this is a separate
engine from `Subset`/`TokenMatcher`, not an extension of either):

```scala
import halotukozak.regex.CaptureMatcher

val date = CaptureMatcher.parse("(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})").toOption.get
val result = date.matchWhole("2026-08-27").get

result.group("year") // Some((start = 0, end = 4))
result.group(1) // same span, by number instead of name
```

`CaptureMatcher` also works as a `case` pattern (`unapplySeq`), one element per numbered group,
`None` for a group that didn't participate rather than `scala.util.matching.Regex`'s `null`:

```scala
"2026-08-27" match
  case date(year, month, day) => println(s"$year/$month/$day")
  case _ => println("no match")
```

## Status

Early (`0.x`). The parser deliberately supports a subset of `java.util.regex.Pattern`'s syntax —
see the package scaladoc for exactly what's in and what's out.
