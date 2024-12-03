package me.cyrill.aoc2024.day3.challenge1

import scala.util.matching.Regex
import me.cyrill.aoc2024.day3.multiplyThenSum

def parse(input: String): Iterator[(Int, Int)] =
  val regex: Regex = """(mul\(\d{1,3},\d{1,3}\))""".r
  for
    m <- regex.findAllIn(input)
    nums = m.drop(4).dropRight(1).split(",").map(_.toInt)
  yield (nums(0), nums(1))

def solve(input: String): Int = multiplyThenSum(parse(input))
