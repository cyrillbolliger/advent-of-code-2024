package me.cyrill.aoc2024.day18

import me.cyrill.aoc2024.day18.*

class Day18Test extends munit.FunSuite:
  val testInput = Array(
    "5,4",
    "4,2",
    "4,5",
    "3,0",
    "2,1",
    "6,3",
    "2,4",
    "1,5",
    "0,6",
    "3,3",
    "2,6",
    "5,1",
    "1,2",
    "5,5",
    "2,5",
    "6,5",
    "1,4",
    "0,4",
    "6,4",
    "1,1",
    "6,1",
    "1,0",
    "0,5",
    "1,6",
    "2,0"
  )

  test("solve1"):
    assertEquals(solve1(testInput.take(12), 7, 7, (0, 0), (6, 6)), 22)

  test("solve2"):
    assertEquals(solve2(testInput, 7, 7, (0, 0), (6, 6), 0), "6,1")
