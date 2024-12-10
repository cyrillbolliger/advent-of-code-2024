package me.cyrill.aoc2024.day9

import me.cyrill.aoc2024.day9.*
import challenge1.*
import Block.*

class Day9Test extends munit.FunSuite:
  val testInput = "2333133121414131402"

  test("expand"):
    assertEquals(
      expand("12345"),
      Vector(
        Used(1, 0),
        Free(2),
        Free(2),
        Used(3, 1),
        Used(3, 1),
        Used(3, 1),
        Free(4),
        Free(4),
        Free(4),
        Free(4),
        Used(5, 2),
        Used(5, 2),
        Used(5, 2),
        Used(5, 2),
        Used(5, 2)
      )
    )

  test("defragment"):
    assertEquals(
      defragment(
        Seq(
          Used(1, 0),
          Free(2),
          Free(2),
          Used(3, 1),
          Used(3, 1),
          Used(3, 1),
          Free(4),
          Free(4),
          Free(4),
          Free(4),
          Used(5, 2),
          Used(5, 2),
          Used(5, 2),
          Used(5, 2),
          Used(5, 2)
        )
      ),
      Vector(
        Used(1, 0),
        Used(5, 2),
        Used(5, 2),
        Used(3, 1),
        Used(3, 1),
        Used(3, 1),
        Used(5, 2),
        Used(5, 2),
        Used(5, 2)
      )
    )

  test("checksum"):
    assertEquals(
      checksum(
        Seq(
          Used(1, 0),
          Used(2, 2),
          Used(2, 2),
          Used(3, 1),
          Used(3, 1),
          Used(3, 1),
          Used(3, 2),
          Used(3, 2),
          Used(3, 2)
        )
      ),
      60L
    )

  test("challenge1.solve"):
    assertEquals(challenge1.solve(testInput), 1928L)

  test("challenge2.solve"):
    assertEquals(challenge2.solve(testInput), 2858L)
    assertEquals(challenge2.solve("12101"), 4L)
    assertEquals(challenge2.solve("12102"), 9L)
    assertEquals(challenge2.solve("1210101"), 10L)
    assertEquals(challenge2.solve("11201"), 7L)
    assertEquals(challenge2.solve("111111201"), 51L)
