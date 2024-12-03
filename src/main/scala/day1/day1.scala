package me.cyrill.aoc2024.day1

import scala.util.matching.Regex
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val inputPath = "src/main/scala/day1/input.txt"

val parseRegex: Regex = """^(\d+)\D+(\d+)$""".r

def parse(line: String): (Int, Int) =
  parseRegex.findFirstMatchIn(line.strip()) match
    case Some(m) => (m.group(1).toInt, m.group(2).toInt)
    case None    => throw new Exception(s"Could not parse line: $line")

def getInput(): (Array[Int], Array[Int]) =
  var a1 = ArrayBuffer[Int]()
  var a2 = ArrayBuffer[Int]()

  for (line <- Source.fromFile(inputPath).getLines())
    val (x, y) = parse(line)
    a1.addOne(x)
    a2.addOne(y)

  (a1.toArray, a2.toArray)

lazy val (a1, a2) = getInput()
lazy val sorted1 = a1.sortInPlace()
lazy val sorted2 = a2.sortInPlace()

def getTotalDistance(): Int =
  sorted1.zip(sorted2).foldLeft(0)((acc, curr) => acc + (curr._1 - curr._2).abs)

def getSimilarityScore(): Int =
  val freq = sorted2.groupBy(identity).view.mapValues(_.length)
  sorted1.foldLeft(0)((acc, curr) => acc + curr * freq.getOrElse(curr, 0))
