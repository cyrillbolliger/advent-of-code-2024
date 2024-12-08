package me.cyrill.aoc2024.day7

import me.cyrill.aoc2024.day7.challenge1.*

class Day7Test extends munit.FunSuite:
  val testInput = Array(
    "190: 10 19", // true
    "3267: 81 40 27", // true
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20" // true
  )

  test("parse"):
    assertEquals(
      parse(testInput(0)),
      (List(BigInt(10), BigInt(19)), BigInt(190))
    )
    assertEquals(
      parse(testInput(1)),
      (List(BigInt(81), BigInt(40), BigInt(27)), BigInt(3267))
    )

  test("combinations"):
    val res = Set(
      List('+', '+', '+'),
      List('*', '+', '+'),
      List('+', '*', '+'),
      List('+', '+', '*'),
      List('*', '*', '+'),
      List('+', '*', '*'),
      List('*', '+', '*'),
      List('*', '*', '*')
    )
    assert(
      combinations(3).toSet.diff(res).isEmpty,
      combinations(3)
    )
    assert(
      res.diff(combinations(3).toSet).isEmpty,
      combinations(3)
    )

  test("eval"):
    assertEquals(eval(List(1, 1), List('+')), BigInt(2))
    assertEquals(eval(List(1, 1), List('*')), BigInt(1))
    assertEquals(eval(List(1, 1, 1), List('+', '+')), BigInt(3))
    assertEquals(eval(List(1, 1, 1), List('+', '*')), BigInt(2))
    assertEquals(eval(List(1, 1, 1), List('*', '+')), BigInt(2))
    assertEquals(eval(List(1, 1, 1), List('*', '*')), BigInt(1))
    assertEquals(eval(List(1, 2, 3), List('+', '+')), BigInt(6))
    assertEquals(eval(List(1, 2, 3), List('+', '*')), BigInt(9))
    assertEquals(eval(List(1, 2, 3), List('*', '+')), BigInt(5))
    assertEquals(eval(List(1, 2, 3), List('*', '*')), BigInt(6))

  test("hasSolution"):
    assert(hasSolution(List(10, 19), 190))
    assert(hasSolution(List(81, 40, 27), 3267))
    assert(hasSolution(List(11, 6, 16, 20), 292))
    assert(!hasSolution(List(15, 6), 156))

  test("solve"):
    assertEquals(solve(testInput), BigInt(3749))
