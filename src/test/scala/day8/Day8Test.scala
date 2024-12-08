package me.cyrill.aoc2024.day8

import me.cyrill.aoc2024.day8.*
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

  test("challenge1.antinodes: 1"):
    val combo = ((2, 0), (1, 2))
    val result = ((3, -2), (0, 4))
    assert(
      challenge1.antinodes(combo) == result
        || challenge1.antinodes(combo) == result.swap
    )

  test("challenge1.antinodes: 2"):
    val combo = ((1, 0), (2, 1))
    val result = ((0, -1), (3, 2))
    assert(
      challenge1.antinodes(combo) == result
        || challenge1.antinodes(combo) == result.swap
    )

  test("challenge1.solve"):
    assertEquals(challenge1.solve(testInput), 14)

  test("challenge2.antinodes: 1"):
    val isOnMap = onMap((3, 5))
    val combo = ((2, 0), (1, 2))
    val result = Set((2, 0), (1, 2), (0, 4))
    assert(challenge2.antinodes(isOnMap)(combo).toSet == result)

  test("challenge2.antinodes: 2"):
    val isOnMap = onMap((5, 6))
    val combo = ((1, 2), (2, 3))
    val result = Set((1, 2), (2, 3), (0, 1), (3, 4), (4, 5))
    assert(challenge2.antinodes(isOnMap)(combo).toSet == result)

  test("challenge2.solve"):
    assertEquals(challenge2.solve(testInput), 34)
