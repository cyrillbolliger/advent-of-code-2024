package me.cyrill.aoc2024.day12

import me.cyrill.aoc2024.day12.*

extension (input: Array[String])
  def toPositionSet(char: Char) = {
    input.zipWithIndex
      .flatMap((row, y) => row.zipWithIndex.map((c, x) => (c, (x, y))))
      .filter((c, pos) => c == char)
      .map(_._2)
      .toSet
  }.ensuring(_.size == input.reduce(_ + _).count(_ == char))

class Day12Test extends munit.FunSuite:
  val testInput = Array(
    "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF",
    "VVRCCCJFFF",
    "VVVVCJJCFE",
    "VVIVCCJJEE",
    "VVIIICJJEE",
    "MIIIIIJJEE",
    "MIIISIJEEE",
    "MMMISSJEEE"
  )

  test("solve"):
    assertEquals(solve(testInput, _.price), 1930)
    assertEquals(solve(testInput, _.discountPrice), 1206)

  test("Garden.findRegion: 3 V"):
    val input = Array(
      "X-X",
      "-X-"
    )

    assertEquals(Garden(parse(input)).findRegion((0, 0)), Set((0, 0)))
    assertEquals(Garden(parse(input)).findRegion((1, 1)), Set((1, 1)))
    assertEquals(Garden(parse(input)).findRegion((2, 0)), Set((2, 0)))

  test("Garden.findRegion: 1 W"):
    val input = Array(
      "X-X-X",
      "XXXXX"
    )

    assertEquals(
      Garden(parse(input)).findRegion((0, 0)),
      input.toPositionSet('X')
    )

  test("Garden.findRegion: 1 L"):
    val input = Array(
      "X----",
      "X----",
      "X----",
      "X----",
      "X----",
      "XXXXX"
    )

    assertEquals(
      Garden(parse(input)).findRegion((3, 5)),
      input.toPositionSet('X')
    )

  test("Region.neighbours"):
    assertEquals(Region(Set()).neighbours((0, 0)), Set())
    assertEquals(
      Region(Set((0, 0), (0, 1), (1, 1))).neighbours((0, 0)),
      Set((0, 1))
    )
    assertEquals(
      Region(Set((0, 0), (0, 1), (1, 1))).neighbours((0, 1)),
      Set((0, 0), (1, 1))
    )

  test("Region.area"):
    val input = Array(
      "-X--",
      "XXXX"
    ).toPositionSet('X')

    assertEquals(Region(input).area, 5)

  test("Region.perimeter"):
    val input = Array(
      "-X--",
      "XXXX"
    ).toPositionSet('X')

    assertEquals(Region(input).perimeter, 12)

  test("Region.sides: 4 ."):
    val input = Array(
      "X"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 4)

  test("Region.sides: 4 @"):
    val input = Array(
      "---",
      "-X-",
      "---"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 4)

  test("Region.sides: 4 rectangle"):
    val input = Array(
      "XX"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 4)

  test("Region.sides: 4 big square"):
    val input = Array(
      "XXX",
      "XXX",
      "XXX"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 4)

  test("Region.sides: 6 L"):
    val input = Array(
      "X-",
      "XX"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 6)

  test("Region.sides: 8 Z"):
    val input = Array(
      "XX-",
      "-XX"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 8)

  test("Region.sides: 12 +"):
    val input = Array(
      "-X-",
      "XXX",
      "-X-"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 12)

  test("Region.sides: 8 O"):
    val input = Array(
      "XXX",
      "X-X",
      "XXX"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 8)

  test("Region.sides: 8 U"):
    val input = Array(
      "X-X",
      "XXX"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 8)

  test("Region.sides: 12 W"):
    val input = Array(
      "X-X-X",
      "XXXXX"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 12)

  test("Region.sides: 12 S"):
    val input = Array(
      "XXXX",
      "XX-X",
      "X-XX",
      "XXXX"
    ).toPositionSet('X')

    assertEquals(Region(input).sides, 12)

  test("Region.sides: example"):
    val input = Array(
      "XXXXXX",
      "XXXBBX",
      "XXXBBX",
      "XCCXXX",
      "XCCXXX",
      "XXXXXX"
    )

    val x = input.toPositionSet('X')
    val b = input.toPositionSet('B')
    val c = input.toPositionSet('C')

    assertEquals(Region(x).sides, 12)
    assertEquals(Region(b).sides, 4)
    assertEquals(Region(c).sides, 4)
