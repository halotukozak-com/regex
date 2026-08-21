package halotukozak.regex

class CharSetTest extends munit.FunSuite:

  test("contains decides membership") {
    val s = CharSet.range('a', 'z')
    assert(s.contains('a'.toInt))
    assert(s.contains('z'.toInt))
    assert(s.contains('m'.toInt))
    assert(!s.contains('A'.toInt))
    assert(!s.contains(('a' - 1).toInt))
  }

  test("union merges overlapping ranges") {
    val a = CharSet.range('a', 'm')
    val b = CharSet.range('h', 'z')
    val u = a.union(b)
    assertEquals(u.ranges, Vector(Range('a', 'z')))
  }

  test("union keeps disjoint ranges separate") {
    val a = CharSet.range('a', 'c')
    val b = CharSet.range('x', 'z')
    val u = a.union(b)
    assertEquals(u.ranges, Vector(Range('a', 'c'), Range('x', 'z')))
  }

  test("union merges adjacent ranges") {
    val a = CharSet.range('a', 'c')
    val b = CharSet.range('d', 'f')
    val u = a.union(b)
    assertEquals(u.ranges, Vector(Range('a', 'f')))
  }

  test("intersect computes overlap") {
    val a = CharSet.range('a', 'm')
    val b = CharSet.range('h', 'z')
    assertEquals(a.intersect(b).ranges, Vector(Range('h', 'm')))
  }

  test("intersect of disjoint is empty") {
    val a = CharSet.range('a', 'c')
    val b = CharSet.range('x', 'z')
    assert(a.intersect(b).isEmpty)
  }

  test("complement of all is empty, of empty is all") {
    assert(CharSet.all.complement.isEmpty)
    assertEquals(CharSet.empty.complement, CharSet.all)
  }

  test("complement excludes original ranges") {
    val s = CharSet.range('a', 'z').complement
    assert(!s.contains('a'.toInt))
    assert(!s.contains('z'.toInt))
    assert(s.contains('A'.toInt))
    assert(s.contains(('a' - 1).toInt))
    assert(s.contains(('z' + 1).toInt))
  }

  test("normalize merges overlapping and adjacent ranges") {
    val s = CharSet.normalize(Range('a', 'c'), Range('b', 'e'), Range('f', 'h'))
    assertEquals(s.ranges, Vector(Range('a', 'h')))
  }

  test("normalize sorts unsorted input") {
    val s = CharSet.normalize(Range('x', 'z'), Range('a', 'c'))
    assertEquals(s.ranges, Vector(Range('a', 'c'), Range('x', 'z')))
  }

  test("dotDefault excludes line terminators") {
    assert(!CharSet.dotDefault.contains('\n'.toInt))
    assert(!CharSet.dotDefault.contains('\r'.toInt))
    assert(!CharSet.dotDefault.contains(0x85))
    assert(!CharSet.dotDefault.contains(0x2028))
    assert(!CharSet.dotDefault.contains(0x2029))
    assert(CharSet.dotDefault.contains('a'.toInt))
  }

  test("empty set contains nothing") {
    assert(!CharSet.empty.contains('a'.toInt))
    assert(!CharSet.empty.contains(0))
    assert(CharSet.empty.isEmpty)
  }

  test("all set contains every code point in range") {
    assert(CharSet.all.contains(0))
    assert(CharSet.all.contains('a'.toInt))
    assert(CharSet.all.contains(CharSet.maxCodePoint))
  }

  test("union is commutative") {
    val a = CharSet.range('a', 'm')
    val b = CharSet.range('h', 'z')
    assertEquals(a.union(b), b.union(a))
  }

  test("union with self is self") {
    val a = CharSet.range('a', 'z')
    assertEquals(a.union(a), a)
  }

  test("union with empty is self") {
    val a = CharSet.range('a', 'z')
    assertEquals(a.union(CharSet.empty), a)
    assertEquals(CharSet.empty.union(a), a)
  }

  test("intersect is commutative") {
    val a = CharSet.range('a', 'm')
    val b = CharSet.range('h', 'z')
    assertEquals(a.intersect(b), b.intersect(a))
  }

  test("intersect with self is self") {
    val a = CharSet.range('a', 'z')
    assertEquals(a.intersect(a), a)
  }

  test("intersect with empty is empty") {
    val a = CharSet.range('a', 'z')
    assert(a.intersect(CharSet.empty).isEmpty)
    assert(CharSet.empty.intersect(a).isEmpty)
  }

  test("intersect with all is self") {
    val a = CharSet.range('a', 'z')
    assertEquals(a.intersect(CharSet.all), a)
  }

  test("double complement returns to original") {
    val a = CharSet.range('a', 'z')
    assertEquals(a.complement.complement, a)
  }

  test("normalize merges multiple adjacent ranges") {
    val s = CharSet.normalize(
      Range('a', 'b'),
      Range('c', 'd'),
      Range('e', 'f'),
    )
    assertEquals(s.ranges, Vector(Range('a', 'f')))
  }

  test("normalize keeps non-overlapping ranges separate") {
    val s = CharSet.normalize(
      Range('a', 'c'),
      Range('f', 'h'),
      Range('m', 'p'),
    )
    assertEquals(
      s.ranges,
      Vector(
        Range('a', 'c'),
        Range('f', 'h'),
        Range('m', 'p'),
      ),
    )
  }

  test("normalize handles duplicate ranges") {
    val s = CharSet.normalize(Range('a', 'z'), Range('a', 'z'))
    assertEquals(s.ranges, Vector(Range('a', 'z')))
  }

  test("single from Int and Char agree") {
    assertEquals(CharSet.single('a'), CharSet.single('a'.toInt))
  }
