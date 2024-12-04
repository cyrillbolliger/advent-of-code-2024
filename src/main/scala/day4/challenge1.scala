package me.cyrill.aoc2024.day4.challenge1

import me.cyrill.aoc2024.day4.*

def countInMatrix(m: Matrix): Int =
  Seq(
    m.hMatch,
    m.hRevMatch,
    m.vMatch,
    m.vRevMatch,
    m.tlBrMatch,
    m.tlBrRevMatch,
    m.trBlMatch,
    m.trBlRevMatch
  )
    .count(p => p)

def countInRows(r0: String, r1: String, r2: String, r3: String): Int =
  val mb = MatrixBuffer(r0, r1, r2, r3)
  (0 until mb.size).map(i => countInMatrix(mb(i))).sum

def countInStringIterator(input: Iterator[String]): Int =
  var matches = 0

  val rb = RowBuffer(input.next(), input.next(), input.next(), input.next())

  while input.hasNext do
    matches += countInRows(rb(0), rb(1), rb(2), rb(3))
    rb.push(input.next)

  matches += countInRows(rb(0), rb(1), rb(2), rb(3))

  // continue counting until the row buffer is eaten up
  for i <- 0 until 3 do
    rb.push("")
    matches += countInRows(rb(0), rb(1), rb(2), rb(3))

  matches
