package me.cyrill.aoc2024.day20

import scala.io.Source
import me.cyrill.aoc2024.util.matrix.Matrix
import me.cyrill.aoc2024.util.pos.Pos

val inputPath = "src/main/scala/day20/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

type World = Matrix[Char, Vector]

def parse(input: Array[String]): World =
  new Matrix(input.map(_.toArray.toVector).toVector)

lazy val worldMap = parse(input)
given world: World = worldMap

def getPos(c: Char)(using m: World): Pos = m.posWhere(_ == c).get

def getPath(startPos: Pos, endPos: Pos)(using m: World): Vector[Pos] =
  var track = List(startPos)
  while track.head != endPos do
    val before = if track.tail.isEmpty then track.head else track.tail.head
    val next =
      m.adjecentWithPos(track.head).filter((p, c) => c != '#' && p != before)
    if next.size != 1 then
      throw Exception(f"Failed to find path at pos: ${track.head}")
    track = next.head._1 :: track
  track.reverse.toVector

def findCheats(p: Vector[Pos], minDist: Int)(using m: World): Set[List[Pos]] =
  (for
    (p1, idx) <- p.zipWithIndex
    p2 <- p.drop(idx + minDist)
    (x1, y1) = p1
    (x2, y2) = p2
    dx = x1 - x2
    dy = y1 - y2
    if (dx == 0 && dy <= 3 && dy >= -3) || (dy == 0 && dx <= 3 && dx >= -3)
    dxOne = if dx == 0 then 0 else 1 * dx.sign
    dyOne = if dy == 0 then 0 else 1 * dy.sign
    nextX = if dx == 0 then x1 else x1 - dxOne
    nextY = if dy == 0 then y1 else y1 - dyOne
    next = m(nextX, nextY)
    if next == '#'
  yield List((x1 - dxOne, y1 - dyOne), (x1 - dxOne * 2, y1 - dyOne * 2))).toSet

def solve1: Int =
  val path = getPath(getPos('S'), getPos('E'))
  val cheats = findCheats(path, 101)
  cheats.size

def solve2: Int = ???
