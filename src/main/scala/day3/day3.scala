package me.cyrill.aoc2024.day3

import scala.io.Source
import me.cyrill.aoc2024.day3

val inputPath = "src/main/scala/day3/input.txt"
val input = Source.fromFile(inputPath).getLines().mkString.trim()

def multiplyThenSum(nums: Iterator[(Int, Int)]): Int =
  nums.foldLeft(0)((acc, ops) => acc + (ops._1 * ops._2))

def solve1: Int = challenge1.solve(input)
def solve2: Int = challenge2.solve(input)
