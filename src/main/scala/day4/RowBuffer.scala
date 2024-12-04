package me.cyrill.aoc2024.day4

class RowBuffer(rows: String*):
  private val data = rows.toArray
  private val dimension = rows.length

  private var start = 0

  def apply(i: Int): String =
    require(i >= 0)
    require(i < dimension)
    data((i + start) % dimension)

  def push(r: String): Unit =
    data(start) = r
    start = (start + 1) % dimension
