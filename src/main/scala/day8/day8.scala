package me.cyrill.aoc2024.day8

import scala.io.Source

type Pos = (Int, Int) // x, y

val inputPath = "src/main/scala/day8/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

def isAntenna(c: Char): Boolean =
  (c >= '0' && c <= '9') ||
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z')

def antennas(input: Array[String]): Iterable[Set[Pos]] =
  input.zipWithIndex
    .flatMap((l, row) =>
      l.toArray.zipWithIndex
        .filter((c, _) => isAntenna(c))
        .map((antenna, col) => (antenna, (col, row)))
    )
    .groupMap(_._1)(_._2)
    .values
    .map(_.toSet)

def mapSize(input: Array[String]): (Int, Int) =
  require(!input.isEmpty)
  (input(0).length, input.length)

def onMap(mapSize: (Int, Int))(pos: (Int, Int)): Boolean =
  val (mx, my) = mapSize
  val (px, py) = pos
  px >= 0 && py >= 0 && px < mx && py < my

def combinations(antennas: Set[Pos]): Set[(Pos, Pos)] =
  val positions = antennas.toList
  val combos = for
    i <- 0 until (positions.size - 1)
    j <- (i + 1) until positions.size
  yield (positions(i), positions(j))
  combos.toSet

def solve1 = challenge1.solve(input)
def solve2 = challenge2.solve(input)
