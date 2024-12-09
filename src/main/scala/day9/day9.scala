package me.cyrill.aoc2024.day9

import scala.io.Source

val inputPath = "src/main/scala/day9/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray.apply(0)

def solve1 = challenge1.solve(input)
// def solve2 = challenge2.solve(input)
