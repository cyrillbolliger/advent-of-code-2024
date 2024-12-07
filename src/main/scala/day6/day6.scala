package me.cyrill.aoc2024.day6

import Field.*
import Orientation.*
import scala.io.Source

val inputPath = "src/main/scala/day6/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

def getNextPos(currentPos: (Int, Int), orientation: Orientation): (Int, Int) =
  orientation match
    case Up    => (currentPos._1, currentPos._2 - 1)
    case Right => (currentPos._1 + 1, currentPos._2)
    case Down  => (currentPos._1, currentPos._2 + 1)
    case Left  => (currentPos._1 - 1, currentPos._2)

def solve1 = challenge1.solve(input)
def solve2 = challenge2.solve(input)
