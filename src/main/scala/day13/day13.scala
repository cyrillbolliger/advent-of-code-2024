package me.cyrill.aoc2024.day13

import scala.io.Source
import scala.util.matching.Regex
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.pos.capped

val A_COSTS = 3
val B_COSTS = 1

val inputPath = "src/main/scala/day13/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

val regex =
  """Button A: X\+(\d+), Y\+(\d+);Button B: X\+(\d+), Y\+(\d+);Prize: X=(\d+), Y=(\d+)""".r

def parse(input: Array[String]): Seq[(Long, Long, Long, Long, Long, Long)] =
  val definitions =
    for i <- 0 until input.length by 4
    yield (i, input(i) + ";" + input(i + 1) + ";" + input(i + 2))

  definitions.map((i, l) =>
    regex.findFirstMatchIn(l) match
      case Some(m) =>
        (
          m.group(1).toLong,
          m.group(2).toLong,
          m.group(3).toLong,
          m.group(4).toLong,
          m.group(5).toLong,
          m.group(6).toLong
        )
      case None =>
        throw new Exception(s"Failed to parse lines: ${i + 1} - ${i + 3}")
  )

case class EquationSystem(
    a1: Long,
    a2: Long,
    b1: Long,
    b2: Long,
    c1: Long,
    c2: Long
):
  val d = a1 * b2 - a2 * b1

  def solve: Option[(Long, Long)] =
    if d == 0 then solveSingular
    else solveUnique

  def solveUnique: Option[(Long, Long)] =
    val dx = c1 * b2 - c2 * b1
    val dy = a1 * c2 - a2 * c1

    val x = Math.round(dx.toDouble / d)
    val y = Math.round(dy.toDouble / d)

    val hasSolution = x * a1 + y * b1 == c1 && x * a2 + y * b2 == c2

    if hasSolution && x >= 0 && y >= 0 then Some((x, y))
    else None

  def solveSingular: Option[(Long, Long)] =
    throw Exception("Singular equation solving not implemented")

def solve(in: Seq[(Long, Long, Long, Long, Long, Long)], add: Long): Long =
  in
    .map((a1, a2, b1, b2, c1, c2) =>
      EquationSystem(a1, a2, b1, b2, c1 + add, c2 + add).solve
    )
    .map(_ match
      case None         => 0L
      case Some((a, b)) => a * A_COSTS + b * B_COSTS
    )
    .sum

def solve1: Long = solve(parse(input), 0)
def solve2: Long = solve(parse(input), 10000000000000L)
