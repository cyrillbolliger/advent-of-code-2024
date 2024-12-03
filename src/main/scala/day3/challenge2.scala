package me.cyrill.aoc2024.day3.challenge2

import scala.util.matching.Regex
import scala.util.matching.Regex.MatchIterator
import me.cyrill.aoc2024.day3.multiplyThenSum

def parse(input: String): MatchIterator =
  val regex: Regex = """(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don't\(\))""".r
  regex.findAllIn(input)

val mulRegex: Regex = """^mul\((\d{1,3}),(\d{1,3})\)$""".r

def parseMul(str: String): (Int, Int) =
  mulRegex.findFirstMatchIn(str) match
    case None =>
      throw IllegalArgumentException(f"Not a mul instruction: $str")
    case Some(m) => (m.group(1).toInt, m.group(2).toInt)

def getMultiplicants(l: Iterator[String]): Iterator[(Int, Int)] =
  var enabled = true
  l.filter(cmd =>
    if cmd == "do()" then
      enabled = true
      false
    else if cmd == "don't()" then
      enabled = false
      false
    else enabled
  ).map(parseMul)

def solve(input: String): Int = multiplyThenSum(getMultiplicants(parse(input)))
