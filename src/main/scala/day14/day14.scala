package me.cyrill.aoc2024.day14

import scala.io.Source
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*

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

def isSymmetric(bots: Vector[Pos]): Boolean =
  bots.forall(_ match
    case (x, _) if x >= WIDTH / 2 => true
    case (x, y)                   => bots.contains((WIDTH - x - 1, y))
  )

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

def neighbourCount(bots: Vector[Pos]): Int =
  bots
    .map(b =>
      (if bots.contains(b.north) then 1 else 0)
        + (if bots.contains(b.east) then 1 else 0)
        + (if bots.contains(b.south) then 1 else 0)
        + (if bots.contains(b.west) then 1 else 0)
    )
    .sum

def solve1: Int = solve1(input)
def solve2: Int =
  val initial = input.map(parse).toVector
  val (_, seconds) = (1 to 10000)
    .map(i => (initial.map(simulate(_, _, i)), i))
    .map((bots, i) => (neighbourCount(bots), i))
    .max

  // val m = MutableMatrix[Char](Array.fill(HEIGHT, WIDTH)('.'))
  // initial.map(simulate(_, _, i)).foreach(m.update(_, 'X'))
  // println(f"--- $i ---")
  // println(m.toString)

  seconds
