package me.cyrill.aoc2024.day4

import me.cyrill.aoc2024.day4.Matrix

class MatrixBuffer(
    private val rows: String*
):
  val size = rows.map(_.length()).max

  private val dimension = rows.length

  private def getCharWithDefault(s: String, default: Char)(n: Int): Char =
    if s.length() > n then s(n) else default

  private def getRow(row: Int, col: Int) =
    require(row >= 0, f"Invalid row: $row. Min: 0")
    require(row < dimension, f"Invalid row: $row. Max: ${dimension - 1}")
    val s = getCharWithDefault(rows(row), '.')
    for i <- (0 until dimension).toArray
    yield s(col + i)

  def apply(n: Int): Matrix =
    for i <- (0 until dimension).toArray
    yield getRow(i, n)
