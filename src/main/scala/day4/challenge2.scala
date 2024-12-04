package me.cyrill.aoc2024.day4.challenge2

import me.cyrill.aoc2024.day4.*

def countInRows(r0: String, r1: String, r2: String): Int =
  val mb = MatrixBuffer(r0, r1, r2)
  (0 until (mb.size - 2)).count(i => mb(i).xMatch)

def countInStringIterator(input: Iterator[String]): Int =
  var matches = 0

  val rb = RowBuffer(input.next(), input.next(), input.next())

  while input.hasNext do
    matches += countInRows(rb(0), rb(1), rb(2))
    rb.push(input.next)

  matches += countInRows(rb(0), rb(1), rb(2))

  matches
