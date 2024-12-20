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

  test("move: simple"):
    val input = Array(
      "########",
      "#..O.O.#",
      "##@.O..#",
      "#...O..#",
      "#.#.O..#",
      "#...O..#",
      "#......#",
      "########",
      "",
      "<^^>>>vv<v>>v<<"
    )

    val state = parseWarehouse(input)
    val moves = parseMoves(input)

    move(state, moves(0))
    assertEquals(
      state.toString(),
      "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########"
    )

    move(state, moves(1))
    assertEquals(
      state.toString(),
      "########\n#.@O.O.#\n##..O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########"
    )

    move(state, moves(2))
    move(state, moves(3))
    assertEquals(
      state.toString(),
      "########\n#..@OO.#\n##..O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########"
    )

    move(state, moves(4))
    assertEquals(
      state.toString(),
      "########\n#...@OO#\n##..O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########"
    )

    move(state, moves(5))
    assertEquals(
      state.toString(),
      "########\n#...@OO#\n##..O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########"
    )

    move(state, moves(6))
    assertEquals(
      state.toString(),
      "########\n#....OO#\n##..@..#\n#...O..#\n#.#.O..#\n#...O..#\n#...O..#\n########"
    )

  test("solve"):
    assertEquals(solve(testInput), 10092)
