package me.cyrill.aoc2024.day8

import scala.io.Source

type Pos = (Int, Int) // x, y

val inputPath = "src/main/scala/day8/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

def solve1 = challenge1.solve(input)
// def solve2 = challenge2.solve(input)
