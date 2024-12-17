package me.cyrill.aoc2024.day14

import scala.io.Source
import me.cyrill.aoc2024.util.pos.*

val WIDTH = 101
val HEIGHT = 103
val ROUNDS = 100

val inputPath = "src/main/scala/day14/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

val regex =
  """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r

def parse(line: String): (Pos, Pos) =
  regex.findFirstMatchIn(line.strip()) match
    case Some(m) =>
      (
        (m.group(1).toInt, m.group(2).toInt),
        (m.group(3).toInt, m.group(4).toInt)
      )
    case None => throw new Exception(s"Could not parse line: $line")

def simulate(p: Pos, v: Pos, rounds: Int): Pos =
  val (x, y) = (p + (v * rounds)) % (WIDTH, HEIGHT)
  (if x < 0 then x + WIDTH else x, if y < 0 then y + HEIGHT else y)

def solve1(input: Array[String]): Int =
  val middle = (WIDTH, HEIGHT) / 2
  input
    .map(parse)
    .map(simulate(_, _, ROUNDS))
    .filter(p => p._1 != middle._1 && p._2 != middle._2)
    .groupBy(_ match
      case (x, y) if x < middle._1 && y < middle._2 => 1
      case (x, y) if x < middle._1 && y > middle._2 => 2
      case (x, y) if x > middle._1 && y < middle._2 => 3
      case (x, y) if x > middle._1 && y > middle._2 => 4
    )
    .view
    .values
    .map(_.size)
    .reduce(_ * _)

def solve1: Int = solve1(input)
