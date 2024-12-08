package me.cyrill.aoc2024.day8.challenge1

import me.cyrill.aoc2024.day8.*

def antinodes(combo: (Pos, Pos)): (Pos, Pos) =
  val (a, b) = combo
  val (xa, ya) = a
  val (xb, yb) = b
  val dx = xb - xa
  val dy = yb - ya
  ((xa - dx, ya - dy), (xb + dx, yb + dy))

def solve(input: Array[String]): Int =
  val isOnMap = onMap(mapSize(input))
  antennas(input)
    .map(combinations)
    .flatMap(combos => combos.flatMap(antinodes(_).toList))
    .filter(isOnMap)
    .toSet
    .size
