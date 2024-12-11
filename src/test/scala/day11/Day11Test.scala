package me.cyrill.aoc2024.day11

import me.cyrill.aoc2024.day11.*

class Day11Test extends munit.FunSuite:
  val testInput = "125 17"

  test("solve1"):
    assertEquals(solve1(testInput, 1), 3)
    assertEquals(solve1(testInput, 2), 4)
    assertEquals(solve1(testInput, 3), 5)
    assertEquals(solve1(testInput, 4), 9)
    assertEquals(solve1(testInput, 5), 13)
    assertEquals(solve1(testInput, 6), 22)
    assertEquals(solve1(testInput, 25), 55312)

  test("parse"):
    assertEquals(parse("123 45 6"), List(Stone(123), Stone(45), Stone(6)))

  test("blink: 0 -> 1"):
    assertEquals(blink(List(Stone(0))), List(Stone(1)))

  test("blink: 2 -> 4048"):
    assertEquals(blink(List(Stone(2))), List(Stone(4048)))

  test("blink: 10 -> 1 0"):
    assertEquals(blink(List(Stone(10))), List(Stone(1), Stone(0)))

  test("blink: 0 23 -> 1 2 3"):
    assertEquals(
      blink(List(Stone(0), Stone(23))),
      List(Stone(1), Stone(2), Stone(3))
    )

  test("iterate"):
    val initial = List(Stone(125), Stone(17))
    val expected = List(
      List(Stone(253000), Stone(1), Stone(7)),
      List(Stone(253), Stone(0), Stone(2024), Stone(14168)),
      List(Stone(512072), Stone(1), Stone(20), Stone(24), Stone(28676032)),
      List(
        Stone(512),
        Stone(72),
        Stone(2024),
        Stone(2),
        Stone(0),
        Stone(2),
        Stone(4),
        Stone(2867),
        Stone(6032)
      ),
      List(
        Stone(1036288),
        Stone(7),
        Stone(2),
        Stone(20),
        Stone(24),
        Stone(4048),
        Stone(1),
        Stone(4048),
        Stone(8096),
        Stone(28),
        Stone(67),
        Stone(60),
        Stone(32)
      ),
      List(
        Stone(2097446912),
        Stone(14168),
        Stone(4048),
        Stone(2),
        Stone(0),
        Stone(2),
        Stone(4),
        Stone(40),
        Stone(48),
        Stone(2024),
        Stone(40),
        Stone(48),
        Stone(80),
        Stone(96),
        Stone(2),
        Stone(8),
        Stone(6),
        Stone(7),
        Stone(6),
        Stone(0),
        Stone(3),
        Stone(2)
      )
    )

    expected.zipWithIndex.foreach((exp, idx) =>
      assertEquals(iterate(initial, idx + 1), exp)
    )
