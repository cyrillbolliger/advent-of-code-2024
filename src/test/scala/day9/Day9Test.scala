package me.cyrill.aoc2024.day9

import me.cyrill.aoc2024.day9.*
import challenge1.*
import Block.*

class Day9Test extends munit.FunSuite:
  val testInput = "2333133121414131402"

  test("expand"):
    assertEquals(
      expand("12345"),
      Seq(
        Used(0),
        Free,
        Free,
        Used(1),
        Used(1),
        Used(1),
        Free,
        Free,
        Free,
        Free,
        Used(2),
        Used(2),
        Used(2),
        Used(2),
        Used(2)
      )
    )

  test("defragment"):
    assertEquals(
      defragment(
        Seq(
          Used(0),
          Free,
          Free,
          Used(1),
          Used(1),
          Used(1),
          Free,
          Free,
          Free,
          Free,
          Used(2),
          Used(2),
          Used(2),
          Used(2),
          Used(2)
        )
      ),
      Seq(
        Used(0),
        Used(2),
        Used(2),
        Used(1),
        Used(1),
        Used(1),
        Used(2),
        Used(2),
        Used(2)
      )
    )

  test("checksum"):
    assertEquals(
      checksum(
        Seq(
          Used(0),
          Used(2),
          Used(2),
          Used(1),
          Used(1),
          Used(1),
          Used(2),
          Used(2),
          Used(2)
        )
      ),
      60L
    )

  test("challenge1.solve"):
    assertEquals(challenge1.solve(testInput), 1928L)
