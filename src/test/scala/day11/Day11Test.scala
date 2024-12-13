package me.cyrill.aoc2024.day11

import me.cyrill.aoc2024.day11.*

class Day11Test extends munit.FunSuite:
  val testInput = "125 17"

  test("solve"):
    assertEquals(solve(testInput, 1), 3L)
    assertEquals(solve(testInput, 2), 4L)
    assertEquals(solve(testInput, 3), 5L)
    assertEquals(solve(testInput, 4), 9L)
    assertEquals(solve(testInput, 5), 13L)
    assertEquals(solve(testInput, 6), 22L)
    assertEquals(solve(testInput, 25), 55312L)

  test("parse"):
    assertEquals(parse("123 45 6"), List(Stone(123), Stone(45), Stone(6)))

  test("change: 0 -> 1"):
    assertEquals(Stone(0).change, List(Stone(1)))

  test("change: 2 -> 4048"):
    assertEquals(Stone(2).change, List(Stone(4048)))

  test("change: 10 -> 1 0"):
    assertEquals(Stone(10).change, List(Stone(1), Stone(0)))

  test("blink: (1, 1) -> (2024, 1)"):
    assertEquals(blink(List(Stone(1) -> 1L)), List(Stone(2024) -> 1L))

  test("blink: (1, 2) -> (2024, 2)"):
    assertEquals(blink(List(Stone(1) -> 2L)), List(Stone(2024) -> 2L))

  test("blink: (2024, 1) -> (20, 1), (24, 1)"):
    assertEquals(
      blink(List(Stone(2024) -> 1L)).toSet,
      Set(Stone(20) -> 1L, Stone(24) -> 1L)
    )

  test("blink: (2024, 2) -> (20, 2), (24, 2)"):
    assertEquals(
      blink(List(Stone(2024) -> 2L)).toSet,
      Set(Stone(20) -> 2L, Stone(24) -> 2L)
    )

  test("blink: (20, 1), (24, 1) -> (2, 2), (0, 1), (4, 1)"):
    assertEquals(
      blink(List(Stone(20) -> 1L, Stone(24) -> 1L)).toSet,
      Set(Stone(2) -> 2L, Stone(0) -> 1L, Stone(4) -> 1L)
    )

  test("iterate"):
    val initial = List(Stone(125) -> 1L, Stone(17) -> 1L)
    val expected = List(
      List(Stone(253000) -> 1L, Stone(1) -> 1L, Stone(7) -> 1L),
      List(
        Stone(253) -> 1L,
        Stone(0) -> 1L,
        Stone(2024) -> 1L,
        Stone(14168) -> 1L
      ),
      List(
        Stone(512072) -> 1L,
        Stone(1) -> 1L,
        Stone(20) -> 1L,
        Stone(24) -> 1L,
        Stone(28676032) -> 1L
      ),
      List(
        Stone(512) -> 1L,
        Stone(72) -> 1L,
        Stone(2024) -> 1L,
        Stone(2) -> 2L,
        Stone(0) -> 1L,
        Stone(4) -> 1L,
        Stone(2867) -> 1L,
        Stone(6032) -> 1L
      ),
      List(
        Stone(1036288) -> 1L,
        Stone(7) -> 1L,
        Stone(2) -> 1L,
        Stone(20) -> 1L,
        Stone(24) -> 1L,
        Stone(4048) -> 2L,
        Stone(1) -> 1L,
        Stone(8096) -> 1L,
        Stone(28) -> 1L,
        Stone(67) -> 1L,
        Stone(60) -> 1L,
        Stone(32) -> 1L
      ),
      List(
        Stone(2097446912) -> 1L,
        Stone(14168) -> 1L,
        Stone(4048) -> 1L,
        Stone(2) -> 4L,
        Stone(0) -> 2L,
        Stone(4) -> 1L,
        Stone(40) -> 2L,
        Stone(48) -> 2L,
        Stone(2024) -> 1L,
        Stone(80) -> 1L,
        Stone(96) -> 1L,
        Stone(8) -> 1L,
        Stone(6) -> 2L,
        Stone(7) -> 1L,
        Stone(3) -> 1L
      )
    )

    expected.zipWithIndex.foreach((exp, idx) =>
      assertEquals(iterate(initial, idx + 1).toSet, exp.toSet)
    )
