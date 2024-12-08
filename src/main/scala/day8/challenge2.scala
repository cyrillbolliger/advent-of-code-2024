package me.cyrill.aoc2024.day8.challenge2

import me.cyrill.aoc2024.day8.*
import scala.collection.mutable.ListBuffer

def antinodes(isOnMap: Pos => Boolean)(combo: (Pos, Pos)): Iterable[Pos] =
  val (a, b) = combo
  val (xa, ya) = a
  val (xb, yb) = b
  val dx = xb - xa
  val dy = yb - ya

  val lb = ListBuffer[Pos](a, b)

  var i = 1
  var antinode = (xa - dx * i, ya - dy * i)

  while isOnMap(antinode) do
    lb.addOne(antinode)
    i += 1
    antinode = (xa - dx * i, ya - dy * i)

  i = 1
  antinode = (xb + dx * i, yb + dy * i)

  while isOnMap(antinode) do
    lb.addOne(antinode)
    i += 1
    antinode = (xb + dx * i, yb + dy * i)

  lb

def solve(input: Array[String]): Int =
  val isOnMap = onMap(mapSize(input))
  antennas(input)
    .map(combinations)
    .flatMap(combos => combos.flatMap(antinodes(isOnMap)(_)))
    .toSet
    .size
