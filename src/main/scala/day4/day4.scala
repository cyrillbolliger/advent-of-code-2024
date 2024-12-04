package me.cyrill.aoc2024.day4

import scala.io.Source

val inputPath = "src/main/scala/day4/input.txt"

def getInput = Source.fromFile(inputPath).getLines()

def solve1: Int = challenge1.countInStringIterator(getInput)
def solve2: Int = challenge2.countInStringIterator(getInput)
