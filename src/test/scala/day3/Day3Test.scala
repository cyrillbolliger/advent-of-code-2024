package me.cyrill.aoc2024.day3

class Day3Test extends munit.FunSuite:
  val data =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

  test("parse: test data"):
    assertEquals(parse(data).toList, List((2, 4), (5, 5), (11, 8), (8, 5)))

  test("multiplyThenSum: ((1, 1), (2, 3)) = 7"):
    val nums = Iterator((1, 1), (2, 3))
    assertEquals(multiplyThenSum(nums), 7)
