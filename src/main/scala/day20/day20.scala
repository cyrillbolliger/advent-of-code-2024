package me.cyrill.aoc2024.day20

import scala.io.Source
import me.cyrill.aoc2024.util.matrix.Matrix
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.astarnode.shortestPath

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

def fragment(a: Pos, b: Pos)(using m: World): World =
  val topLeft = a.topLeft(b)
  val bottomRight = a.bottomRight(b)
  val (dx, dy) = bottomRight - topLeft
  val data = Vector.tabulate(dy + 1, dx + 1)((y, x) => m(topLeft + (x, y)))
  new Matrix(data)

def hasCheatPath(a: Pos, b: Pos)(using m: World): Boolean =
  val world = fragment(a, b)
  val adjecent = (pos) => world.adjecentWithPos(pos)
  val pathF = shortestPath[Pos]((a, b) => a.manhattanDist(b))
  val topLeft = a.topLeft(b)
  val relA = a - topLeft
  val relB = b - topLeft
  val cheatPath = pathF(adjecent(_).map(_._1))(relA, relB)
  val realPath =
    pathF(adjecent(_).filter((_, c) => c != '#').map(_._1))(relA, relB)

  cheatPath.size > 0 && (realPath.size == 0 || realPath.size > cheatPath.size)

def findCheats(p: Vector[Pos], minSaving: Int, maxLen: Int)(using
    m: World
): Set[(Pos, Pos)] =
  (for
    (p1, idx1) <- p.zipWithIndex
    (p2, idx2) <- p.zipWithIndex.drop(idx1 + minSaving + 1)
    dist = p1.manhattanDist(p2)
    if dist <= maxLen && (idx2 - idx1 - dist) >= minSaving
    if hasCheatPath(p1, p2)
  yield (p1, p2)).toSet

lazy val path = getPath(getPos('S'), getPos('E'))

def solve1: Int = findCheats(path, 100, 2).size
def solve2: Int = findCheats(path, 100, 20).size
