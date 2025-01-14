package me.cyrill.aoc2024.day15

import me.cyrill.aoc2024.day15.*

class Day15Test extends munit.FunSuite:
  val testInput = Array(
    "##########",
    "#..O..O.O#",
    "#......O.#",
    "#.OO..O.O#",
    "#..O@..O.#",
    "#O#..O...#",
    "#O..O..O.#",
    "#.OO.O.OO#",
    "#....O...#",
    "##########",
    "",
    "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^",
    "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v",
    "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<",
    "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^",
    "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><",
    "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^",
    ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^",
    "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>",
    "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>",
    "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
  )

  test("challenge1.solve"):
    assertEquals(challenge1.solve(testInput), 10092)

  test("challenge2.move: simple"):
    val input = Array(
      "#######",
      "#...#.#",
      "#.....#",
      "#..OO@#",
      "#..O..#",
      "#.....#",
      "#######",
      "",
      "<vv<<^^<<^^"
    )

    var state = challenge2.parseWarehouse(input)
    val moves = parseMoves(input)

    val expected = List(
      Array(
        "##############",
        "##......##..##",
        "##..........##",
        "##...[][]@..##",
        "##....[]....##",
        "##..........##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##..........##",
        "##...[][]...##",
        "##....[].@..##",
        "##..........##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##..........##",
        "##...[][]...##",
        "##....[]....##",
        "##.......@..##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##..........##",
        "##...[][]...##",
        "##....[]....##",
        "##......@...##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##..........##",
        "##...[][]...##",
        "##....[]....##",
        "##.....@....##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##...[][]...##",
        "##....[]....##",
        "##.....@....##",
        "##..........##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##...[][]...##",
        "##....[]....##",
        "##.....@....##",
        "##..........##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##...[][]...##",
        "##....[]....##",
        "##....@.....##",
        "##..........##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##...[][]...##",
        "##....[]....##",
        "##...@......##",
        "##..........##",
        "##############"
      ),
      Array(
        "##############",
        "##......##..##",
        "##...[][]...##",
        "##...@[]....##",
        "##..........##",
        "##..........##",
        "##############"
      ),
      Array(
        "##############",
        "##...[].##..##",
        "##...@.[]...##",
        "##....[]....##",
        "##..........##",
        "##..........##",
        "##############"
      )
    )

    for i <- 0 until moves.length do
      state = challenge2.move(state, moves(i)).getOrElse(state)
      assertEquals(
        state.toString(),
        expected(i).reduce(_ + "\n" + _),
        f"Move $i failed: ${moves(i)}\nExpected:\n${expected(i)
            .reduce(_ + "\n" + _)}\nGot:\n${state.toString()}"
      )

  test("challenge2.solve"):
    assertEquals(challenge2.solve(testInput), 9021)
