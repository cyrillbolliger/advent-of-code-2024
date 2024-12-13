package me.cyrill.aoc2024.day11

import scala.io.Source

val FACTOR = 2024

val inputPath = "src/main/scala/day11/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray.apply(0)

case class Stone(n: Long):
  lazy val digits: String = n.toString
  lazy val size: Int = digits.size
  lazy val isEven: Boolean = size % 2 == 0

  def change: List[Stone] =
    if n == 0 then List(Stone(1))
    else if isEven then
      List(
        Stone(digits.take(size / 2).toInt),
        Stone(digits.drop(size / 2).toInt)
      )
    else List(Stone(n * FACTOR))

def parse(in: String): List[Stone] =
  in.split(" ").map(s => Stone(s.toInt)).toList

def blink(in: List[(Stone, Long)]): List[(Stone, Long)] =
  in.flatMap((stone, count) => stone.change.map((_, count)))
    .groupMapReduce(_._1)(_._2)(_ + _)
    .toList

def iterate(in: List[(Stone, Long)], iterations: Int): List[(Stone, Long)] =
  if iterations == 0 then in
  else iterate(blink(in), iterations - 1)

def solve(input: String, iterations: Int): Long =
  val stones = parse(input)
  val grouped =
    stones.groupBy(_.n).map((n, l) => (Stone(n), l.size.toLong)).toList
  iterate(grouped, iterations).foldLeft(0L)((acc, grouped) => acc + grouped._2)

def solve1: Long = solve(input, 25)
def solve2: Long = solve(input, 75)
