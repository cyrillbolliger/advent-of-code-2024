package me.cyrill.aoc2024.day13

import scala.io.Source
import scala.util.matching.Regex
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.pos.capped

val MAX_BUTTON_PUSHES = 100
val A_COSTS = 3
val B_COSTS = 1

val inputPath = "src/main/scala/day13/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

val regex =
  """Button A: X\+(\d+), Y\+(\d+);Button B: X\+(\d+), Y\+(\d+);Prize: X=(\d+), Y=(\d+)""".r

def parse(input: Array[String]): Seq[Equation] =
  val definitions =
    for i <- 0 until input.length by 4
    yield (i, input(i) + ";" + input(i + 1) + ";" + input(i + 2))

  definitions.map((i, l) =>
    regex.findFirstMatchIn(l) match
      case Some(m) =>
        Equation(
          (m.group(1).toInt, m.group(2).toInt),
          (m.group(3).toInt, m.group(4).toInt),
          (m.group(5).toInt, m.group(6).toInt)
        )
      case None =>
        throw new Exception(s"Failed to parse lines: ${i + 1} - ${i + 3}")
  )

case class Equation(ba: Pos, bb: Pos, res: Pos):
  val aMax = (res / ba).capped(MAX_BUTTON_PUSHES)
  val bMax = (res / bb).capped(MAX_BUTTON_PUSHES)

  def exceedsMaxPushes: Boolean =
    val sum = (aMax * ba) + (bMax * bb)
    sum._1 < res._1 || sum._2 < res._2

  lazy val solutions: Seq[(Int, Int)] =
    if exceedsMaxPushes then Nil
    else
      for
        a <- 0 to Math.min(aMax._1, aMax._2)
        b <- 0 to Math.min(bMax._1, bMax._2)
        if (ba * a) + (bb * b) == res
      yield (a, b)

  def minimalCosts: Option[Int] =
    solutions match
      case Nil => None
      case _   => Some(solutions.map((a, b) => a * A_COSTS + b * B_COSTS).min)

def solve(equations: Seq[Equation]): Int =
  equations
    .map(_.minimalCosts.getOrElse(0))
    .sum

def solve1: Int = solve(parse(input))
// def solve2: Int = solve(input, _.discountPrice)
