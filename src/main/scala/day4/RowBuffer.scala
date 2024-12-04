package me.cyrill.aoc2024.day4

class RowBuffer(r0: String, r1: String, r2: String, r3: String):
  private val rows = Array(r0, r1, r2, r3)
  private var start = 0

  def apply(i: Int): String =
    require(i >= 0)
    require(i <= 3)
    rows((i + start) % 4)

  def push(r: String): Unit =
    rows(start) = r
    start = (start + 1) % 4
