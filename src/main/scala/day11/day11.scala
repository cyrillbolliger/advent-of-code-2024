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

def blink(in: List[Stone]): List[Stone] = in.flatMap(_.change)

def iterate(in: List[Stone], iterations: Int): List[Stone] =
  if iterations == 0 then in
  else iterate(blink(in), iterations - 1)

def solve1: Int = solve1(input, 25)
def solve1(input: String, iterations: Int): Int =
  iterate(parse(input), iterations).size
