package me.cyrill.aoc2024.day3

import scala.util.matching.Regex
import scala.io.Source

val inputPath = "src/main/scala/day3/input.txt"

def parse(input: String): Iterator[(Int, Int)] =
  val regex: Regex = """(mul\(\d{1,3},\d{1,3}\))""".r
  for
    m <- regex.findAllIn(input)
    nums = m.drop(4).dropRight(1).split(",").map(_.toInt)
  yield (nums(0), nums(1))

def getInput(): String =
  Source.fromFile(inputPath).getLines().mkString.trim()

def multiplyThenSum(nums: Iterator[(Int, Int)]): Int =
  nums.foldLeft(0)((acc, ops) => acc + (ops._1 * ops._2))

def getSumOfMultiplications(): Int =
  multiplyThenSum(parse(getInput()))
