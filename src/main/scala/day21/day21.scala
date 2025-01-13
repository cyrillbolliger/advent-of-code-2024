package me.cyrill.aoc2024.day21

import scala.io.Source
import me.cyrill.aoc2024.util.pos.*

val inputPath = "src/main/scala/day21/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

val keyPad = Map[Char, Pos](
  'A' -> (2, 3),
  '0' -> (1, 3),
  '1' -> (0, 2),
  '2' -> (1, 2),
  '3' -> (2, 2),
  '4' -> (0, 1),
  '5' -> (1, 1),
  '6' -> (2, 1),
  '7' -> (0, 0),
  '8' -> (1, 0),
  '9' -> (2, 0)
)

val dirPad = Map[Char, Pos](
  '^' -> (1, 0),
  'A' -> (2, 0),
  '<' -> (0, 1),
  'v' -> (1, 1),
  '>' -> (2, 1)
)

given dir2dirPathCache: scala.collection.mutable.Map[(Char, Char), String] =
  scala.collection.mutable.Map[(Char, Char), String]()
given encodeDirsCache: scala.collection.mutable.Map[(String, Int), Long] =
  scala.collection.mutable.Map[(String, Int), Long]()

def isAllowed(pad: Map[Char, Pos])(moves: String, start: Char): Boolean =
  moves
    .foldLeft((true, pad(start)))((acc, mv) =>
      val (legal, pos) = acc
      val newPos = mv match
        case 'A' => pos
        case '^' => pos.north
        case '>' => pos.east
        case 'v' => pos.south
        case '<' => pos.west
        case _   => throw IllegalArgumentException(f"Unknown move: $mv")
      (legal && pad.exists((_, p) => p == newPos), newPos)
    )
    ._1

def x2dirPaths(pad: Map[Char, Pos])(from: Char, to: Char): Set[String] =
  val (dx, dy) = pad(to) - pad(from)
  val h = if dx < 0 then "<" else ">"
  val v = if dy < 0 then "^" else "v"
  val dh = h * Math.abs(dx)
  val dv = v * Math.abs(dy)
  val paths = Set(dh ++ dv, dv ++ dh).filter(m => isAllowed(pad)(m, from))
  paths.map(_ ++ "A")

def dir2dirPaths(dirs: String): Set[String] =
  f"A$dirs"
    .zip(dirs)
    .map((from, to) => x2dirPaths(dirPad)(from, to))
    .foldLeft(Set(""))((acc, dirs) => dirs.flatMap(d => acc.map(_ + d)))

def pickOptimal(paths: Set[String]): String =
  if paths.size == 1 then paths.head
  else
    paths
      .map(d0 =>
        val d1 = dir2dirPaths(d0)
        val d2 = d1.flatMap(dir2dirPaths(_))
        (d0, d2.minBy(_.size).size)
      )
      .minBy(_._2) // take path with the shortest final input
      ._1

def num2dirPath(from: Char, to: Char): String =
  pickOptimal(x2dirPaths(keyPad)(from, to))

def dir2dirPath(from: Char, to: Char)(using
    cache: scala.collection.mutable.Map[(Char, Char), String]
): String =
  cache.getOrElseUpdate((from, to), pickOptimal(x2dirPaths(dirPad)(from, to)))

def encodeDirs(in: String, n: Int)(using
    cache: scala.collection.mutable.Map[(String, Int), Long]
): Long =
  if n == 0 then in.size
  else
    cache.getOrElseUpdate(
      (in, n), {
        val parts = in.split("(?<=A)")
        parts.foldLeft(0L)((acc, part) =>
          val once = f"A$part".zip(part).map(dir2dirPath).mkString
          acc + encodeDirs(once, n - 1)
        )
      }
    )

def encodeChar(
    c: Char,
    numPadState: Char,
    keyPadCount: Int
): Long =
  val d0 = num2dirPath(numPadState, c)
  encodeDirs(d0, keyPadCount)

def encodeCode(
    code: String,
    numPadState: Char,
    keyPadCount: Int
): (Long, Char) =
  code
    .foldLeft((0L, numPadState))((acc, c) =>
      val (dIn, nIn) = acc
      val dOut = encodeChar(c, nIn, keyPadCount)
      (dIn + dOut, c)
    )

def encodeCodes(codes: List[String], keyPadCount: Int): List[Long] =
  codes
    .foldLeft((List[Long](), 'A'))((acc, c) =>
      val (dIn, nIn) = acc
      val (dOut, nOut) = encodeCode(c, nIn, keyPadCount)
      (dIn :+ dOut, nOut)
    )
    ._1

def complexity(code: String, len: Long) =
  code.replace("A", "").toLong * len

def solve1: Long = input.zip(encodeCodes(input.toList, 2)).map(complexity).sum
def solve2: Long = input.zip(encodeCodes(input.toList, 25)).map(complexity).sum
