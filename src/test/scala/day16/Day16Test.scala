package me.cyrill.aoc2024.day16

import me.cyrill.aoc2024.day16.*

class Day16Test extends munit.FunSuite:
  val testInput = Array(
    "###############",
    "#.......#....E#",
    "#.#.###.#.###.#",
    "#.....#.#...#.#",
    "#.###.#####.#.#",
    "#.#.#.......#.#",
    "#.#.#####.###.#",
    "#...........#.#",
    "###.#.#####.#.#",
    "#...#.....#.#.#",
    "#.#.#.###.#.#.#",
    "#.....#...#.#.#",
    "#.###.#.#.#.#.#",
    "#S..#.....#...#",
    "###############"
  )

  test("solve1"):
    assertEquals(solve1(testInput), 7036)

  test("solve2"):
    assertEquals(solve2(testInput), 45)
