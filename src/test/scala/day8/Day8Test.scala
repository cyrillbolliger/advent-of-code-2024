package me.cyrill.aoc2024.day8

import me.cyrill.aoc2024.day8.challenge1.*
import scala.compiletime.ops.double

class Day8Test extends munit.FunSuite:
  val testInput = Array(
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
  )

  test("antennas"):
    // format: off
    val input = Array(
      "A.0",
      "..A",
      ".00"
    )
    // format: on
    val result = Iterable(
      Set((2, 0), (1, 2), (2, 2)),
      Set((0, 0), (2, 1))
    )
    assertEquals(antennas(input), result)

  test("mapSize"):
    // format: off
    val input = Array(
      "1234",
      "2...",
      "3..."
    )
    // format: on
    assertEquals(mapSize(input), (4, 3))

  test("isAntenna"):
    for c <- '0' to '9' do assert(isAntenna(c))
    for c <- 'a' to 'z' do assert(isAntenna(c))
    for c <- 'A' to 'Z' do assert(isAntenna(c))
    assert(!isAntenna('.'))
    assert(!isAntenna('#'))

  test("onMap"):
    assert(onMap((2, 3))((1, 2)))
    assert(onMap((2, 3))((0, 0)))
    assert(!onMap((2, 3))((2, 2)))
    assert(!onMap((2, 3))((1, 3)))
    assert(!onMap((2, 3))((0, -1)))
    assert(!onMap((2, 3))((-1, 0)))

  test("combinations"):
    val antennas = Set((0, 2), (2, 1), (2, 2))
    val result = Set(
      ((0, 2), (2, 1)),
      ((0, 2), (2, 2)),
      ((2, 1), (2, 2))
    )
    assert(combinations(antennas).diff(result).isEmpty)
    assert(result.diff(combinations(antennas)).isEmpty)

  test("antinodes: 1"):
    val combo = ((2, 0), (1, 2))
    val result = ((3, -2), (0, 4))
    assert(antinodes(combo) == result || antinodes(combo) == result.swap)

  test("antinodes: 2"):
    val combo = ((1, 0), (2, 1))
    val result = ((0, -1), (3, 2))
    assert(antinodes(combo) == result || antinodes(combo) == result.swap)

  test("solve"):
    assertEquals(solve(testInput), 14)
