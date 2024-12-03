package me.cyrill.aoc2024.day2

import scala.util.matching.Regex
import scala.io.Source

val inputPath = "src/main/scala/day2/input.txt"

def parse(line: String): Array[Int] = line.strip().split(" ").map(_.toInt)

def getInput() =
  for (line <- Source.fromFile(inputPath).getLines())
    yield parse(line)

def verify(x: Int, y: Int, sign: Int): Boolean =
  val diff = x - y
  val d = diff.abs
  diff.sign == sign && d >= 1 && d <= 3

def detectSign(report: Array[Int]): Int =
  val pairs = report.zip(report.tail)
  if pairs.map((x, y) => (x - y).sign).sum > 0 then 1 else -1

def isSafe(report: Array[Int]): Boolean =
  report.length match
    case 0 => false
    case 1 => true
    case _ =>
      val pairs = report.zip(report.tail)
      val sign = detectSign(report)
      pairs.forall((x, y) => verify(x, y, sign))

def isSafeWithTolerance(report: Array[Int]): Boolean =
  if isSafe(report) then true
  else
    val oneOmitted =
      for i <- 0 until report.length
      yield report.slice(0, i) ++ report.slice(i + 1, report.length)
    oneOmitted.exists(isSafe)

def getSafeCount() =
  getInput().filter(isSafe).length

def getSafeCountWithTolerance() =
  getInput().filter(isSafeWithTolerance).length
