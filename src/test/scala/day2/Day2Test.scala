package me.cyrill.aoc2024.day2

class Day2Test extends munit.FunSuite:
  val testSets = List(
    (Array(7, 6, 4, 2, 1), true, true),
    (Array(1, 2, 7, 8, 9), false, false),
    (Array(9, 7, 6, 2, 1), false, false),
    (Array(1, 3, 2, 4, 5), false, true),
    (Array(8, 6, 4, 4, 1), false, true),
    (Array(1, 3, 6, 7, 9), true, true)
  )

  test("isSafe: test sets"):
    testSets.foreach((report, expected, _) =>
      assertEquals(isSafe(report), expected)
    )

  test("isSafeWithTolerance: test sets"):
    testSets.foreach((report, _, expected) =>
      assertEquals(isSafeWithTolerance(report), expected)
    )
