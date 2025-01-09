package me.cyrill.aoc2024.day21

import me.cyrill.aoc2024.day21.*

class Day21Test extends munit.FunSuite:
  def dirs2str(dirs: List[Char]): String = dirs.mkString

  test("num2dirs"):
    val cases = Map(
      ('9', '9') -> Set("A"),
      ('8', '9') -> Set(">A"),
      ('7', '9') -> Set(">>A"),
      ('7', '4') -> Set("vA"),
      ('7', '5') -> Set(">vA", "v>A"),
      ('2', '1') -> Set("<A"),
      ('1', '4') -> Set("^A"),
      ('1', '0') -> Set(">vA"),
      ('0', '1') -> Set("^<A")
    )
    for (t, r) <- cases do assertEquals(num2dirs(t._1, t._2), r)

  test("dir2dirs: basic"):
    val cases = Map(
      ('^', '^') -> "A",
      ('^', 'A') -> ">A",
      ('<', '>') -> ">>A",
      ('^', 'v') -> "vA",
      ('^', '>') -> ">vA",
      ('v', '<') -> "<A",
      ('v', '^') -> "^A"
    )
    for (t, r) <- cases do assertEquals(dir2dirs(t._1, t._2), r)

  test("dir2dirs: escape gap"):
    val cases = Map(
      ('<', '^') -> ">^A",
      ('^', '<') -> "v<A"
    )
    for (t, r) <- cases do assertEquals(dir2dirs(t._1, t._2), r)

  test("encodeChar: A -> 0"):
    val expected = "<vA<AA>>^AvAA<^A>A"
    val (dirs, np, kp1, kp2) = encodeChar('0', 'A', 'A', 'A')

    assertEquals(
      dirs.sorted,
      expected.sorted,
      f"expected: $expected\n     got: $dirs"
    )
    assertEquals(np, '0')
    assertEquals(kp1, 'A')
    assertEquals(kp2, 'A')

  test("encodeChar: 0 -> 2"):
    val expected = "<v<A>>^AvA^A"
    val (dirs, np, kp1, kp2) = encodeChar('2', '0', 'A', 'A')

    assertEquals(
      dirs.sorted,
      expected.sorted,
      f"expected: $expected\n     got: $dirs"
    )
    assertEquals(np, '2')
    assertEquals(kp1, 'A')
    assertEquals(kp2, 'A')

  test("encodeChar: 2 -> 9"):
    val expected = "<vA>^A<v<A>^A>AAvA^A"
    val (dirs, np, kp1, kp2) = encodeChar('9', '2', 'A', 'A')

    assertEquals(
      dirs.sorted,
      expected.sorted,
      f"expected: $expected\n     got: $dirs"
    )
    assertEquals(np, '9')
    assertEquals(kp1, 'A')
    assertEquals(kp2, 'A')

  test("encodeChar: 9 -> A"):
    val expected = "<v<A>A>^AAAvA<^A>A"
    val (dirs, np, kp1, kp2) = encodeChar('A', '9', 'A', 'A')

    assertEquals(
      dirs.sorted,
      expected.sorted,
      f"expected: $expected\n     got: $dirs"
    )
    assertEquals(np, 'A')
    assertEquals(kp1, 'A')
    assertEquals(kp2, 'A')

  test("encodeCode: 029A"):
    val expected =
      "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
    val (dirs, np, kp1, kp2) = encodeCode("029A", 'A', 'A', 'A')

    assertEquals(
      dirs.sorted,
      expected.sorted,
      f"expected: $expected\n     got: $dirs"
    )
    assertEquals(np, 'A')
    assertEquals(kp1, 'A')
    assertEquals(kp2, 'A')

  test("encodeCode: 379A"):
    val expected =
      "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
    val (dirs, np, kp1, kp2) = encodeCode("379A", 'A', 'A', 'A')

    assertEquals(
      dirs.sorted,
      expected.sorted,
      f"expected: $expected\n     got: $dirs"
    )
    assertEquals(np, 'A')
    assertEquals(kp1, 'A')
    assertEquals(kp2, 'A')

  test("encodeCodes"):
    val expected = List(
      "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A",
      "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A",
      "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A",
      "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A",
      "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
    )
    val results = encodeCodes(List("029A", "980A", "179A", "456A", "379A"))

    results
      .zip(expected)
      .zipWithIndex
      .map((t, idx) =>
        assertEquals(
          t._1.sorted,
          t._2.sorted,
          f"code ${results(idx)} failed\nexpected: ${t._2}\n     got: ${t._1}"
        )
      )
