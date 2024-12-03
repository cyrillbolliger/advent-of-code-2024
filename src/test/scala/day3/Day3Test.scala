package me.cyrill.aoc2024.day3

class Day3Test extends munit.FunSuite:
  test("multiplyThenSum: ((1, 1), (2, 3)) = 7"):
    val nums = Iterator((1, 1), (2, 3))
    assertEquals(multiplyThenSum(nums), 7)

  test("Challenge1.parse: test data"):
    val data =
      "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    assertEquals(
      challenge1.parse(data).toList,
      List((2, 4), (5, 5), (11, 8), (8, 5))
    )

  test("Challenge2.parse: test data"):
    val data =
      "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

    assertEquals(
      challenge2.parse(data).toList,
      List(
        "mul(2,4)",
        "don't()",
        "mul(5,5)",
        "mul(11,8)",
        "do()",
        "mul(8,5)"
      )
    )

  test("Challenge2.getMultiplicants: test data"):
    val data = List(
      "mul(2,4)",
      "don't()",
      "mul(5,5)",
      "mul(11,8)",
      "do()",
      "mul(8,5)"
    )

    assertEquals(
      challenge2.getMultiplicants(data.iterator).toList,
      List((2, 4), (8, 5))
    )
