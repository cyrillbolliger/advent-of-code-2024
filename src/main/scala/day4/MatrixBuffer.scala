package me.cyrill.aoc2024.day4

import me.cyrill.aoc2024.day4.Matrix

class MatrixBuffer(
    private val r0: String,
    private val r1: String,
    private val r2: String,
    private val r3: String
):
  private val rows = Array(r0, r1, r2, r3)

  val size = rows.map(_.length()).max

  private def getCharWithDefault(s: String, default: Char)(n: Int): Char =
    if s.length() > n then s(n) else default

  private def getRow(row: Int, col: Int) =
    require(row >= 0, f"Invalid row: $row. Min: 0")
    require(row <= 3, f"Invalid row: $row. Max: 3")
    val s = getCharWithDefault(rows(row), '.')
    Array(
      s(col),
      s(col + 1),
      s(col + 2),
      s(col + 3)
    )

  def apply(i: Int): Matrix =
    Array(
      getRow(0, i),
      getRow(1, i),
      getRow(2, i),
      getRow(3, i)
    )
