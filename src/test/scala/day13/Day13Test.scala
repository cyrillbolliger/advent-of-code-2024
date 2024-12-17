package me.cyrill.aoc2024.day13

import me.cyrill.aoc2024.day13.*

class Day13Test extends munit.FunSuite:
  val testInput = Array(
    "Button A: X+94, Y+34",
    "Button B: X+22, Y+67",
    "Prize: X=8400, Y=5400",
    "",
    "Button A: X+26, Y+66",
    "Button B: X+67, Y+21",
    "Prize: X=12748, Y=12176",
    "",
    "Button A: X+17, Y+86",
    "Button B: X+84, Y+37",
    "Prize: X=7870, Y=6450",
    "",
    "Button A: X+69, Y+23",
    "Button B: X+27, Y+71",
    "Prize: X=18641, Y=10279"
  )

  // test("parse"):
  //   assertEquals(
  //     parse(testInput.take(7)),
  //     Seq(
  //       Equation((94, 34), (22, 67), (8400, 5400)),
  //       Equation((26, 66), (67, 21), (12748, 12176))
  //     )
  //   )

  // test("Equation.minimalCosts: is only defined if there is a solution"):
  //   val equations = parse(testInput)

  //   assert(equations(0).minimalCosts.isDefined)
  //   assert(equations(1).minimalCosts.isEmpty)
  //   assert(equations(2).minimalCosts.isDefined)
  //   assert(equations(3).minimalCosts.isEmpty)

  test("EquationSystem.minimalCosts: first = (80, 40)"):
    val equations = parse(testInput)
    assertEquals(equations(0).solve.get, (80L, 40L))

  test("EquationSystem.minimalCosts: third = (38, 86)"):
    val equations = parse(testInput)
    assertEquals(equations(2).solve.get, (38L, 86L))

  test("solve"):
    assertEquals(solve(parse(testInput)), 480L)
