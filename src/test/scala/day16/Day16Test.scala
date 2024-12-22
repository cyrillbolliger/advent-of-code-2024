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

  test("solve"):
    assertEquals(solve(parse(testInput)), 7036)

  test("Maze.costs"):
    val m = parse(Array("...", ".S.", "..."))
    assertEquals(m.costs(Dir.East), 1)
    assertEquals(m.costs(Dir.North), 1001)
    assertEquals(m.costs(Dir.South), 1001)
    intercept[IllegalArgumentException](m.costs(Dir.West))

  test("Maze.possibleSteps: None"):
    val m = parse(Array(".#.", ".S#", ".#."))
    assertEquals(m.possibleSteps, Nil)

  test("Maze.possibleSteps: Three"):
    val m = parse(Array(".E.", ".S.", "..."))
    assertEquals(
      m.possibleSteps,
      List(
        (Dir.North, (1, 0), 1001, Cell.End(Int.MaxValue)),
        (Dir.East, (2, 1), 1, Cell.Free),
        (Dir.South, (1, 2), 1001, Cell.Free)
      )
    )

  test("Maze.possibleSteps: Cheaper"):
    val m = parse(Array(".#.", ".S.", ".#."))
      .updated((2, 1), Cell.Path(3))
    assertEquals(
      m.possibleSteps,
      List((Dir.East, (2, 1), 1, Cell.Path(3)))
    )

  test("Maze.possibleSteps: More expensive"):
    val m = parse(Array(".#.", ".S.", ".#."))
      .updated((2, 1), Cell.Path(0))
    assertEquals(m.possibleSteps, Nil)

  test("Maze.step: Done"):
    val m = parse(Array(".#.", ".S#", ".#.")).copy(scores = Set(1))
    assertEquals(m.step, m.copy(done = true))

  test("Maze.step: toExplore"):
    val m = parse(Array("...", "#..", "S#."))
      .copy(toExplore = Map(((1, 0), Dir.West), ((2, 0), Dir.South)))
    assertEquals(
      m.step,
      m.copy(
        start = 1,
        orientation = Dir.West,
        toExplore = Map(((2, 0), Dir.South))
      )
    )

  test("Maze.step: Forward 1"):
    val m = parse(Array("S..", "...", "..."))
    assertEquals(
      m.step,
      m.updated((1, 0), Cell.Path(1))
        .copy(
          start = 1,
          toExplore = Map(((0, 0), Dir.East))
        )
    )

  test("Maze.step: Forward 2"):
    val m = parse(Array("S..", "...", "..."))
    assertEquals(
      m.step.step,
      m.updated((1, 0), Cell.Path(1))
        .updated((2, 0), Cell.Path(2))
        .copy(
          start = 2,
          toExplore = Map(((0, 0), Dir.East), ((1, 0), Dir.East))
        )
    )

  test("Maze.step: Forward 1, dead end, explore 1"):
    val m = parse(Array("##.", ".#.", "S.."))
    assertEquals(
      m.step.step.step,
      m.updated((0, 1), Cell.Path(1001))
        .updated((1, 2), Cell.Path(1))
        .copy(
          start = 7,
          toExplore = Map()
        )
    )

  test("Maze.step: Fork and join"):
    var m = parse(Array("....", ".#.#", "S..#"))
    (0 until 11).foreach(_ => m = m.step)
    assertEquals(m(3, 0), Cell.Path(2005))
