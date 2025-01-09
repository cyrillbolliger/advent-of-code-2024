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

def x2dirs(pad: Map[Char, Pos])(from: Char, to: Char): (String, String) =
  val (dx, dy) = pad(to) - pad(from)
  val h = if dx < 0 then "<" else ">"
  val v = if dy < 0 then "^" else "v"
  (h * Math.abs(dx), v * Math.abs(dy))

def num2dirs(from: Char, to: Char): Set[String] =
  val (h, v) = x2dirs(keyPad)(from, to)
  val paths = Set(h ++ v, v ++ h).filter(m => isAllowed(keyPad)(m, from))
  paths.map(_ ++ "A")

def dir2dirs(from: Char, to: Char): String = {
  val (h, v) = x2dirs(dirPad)(from, to)
  val moves = if h.startsWith(">") then h ++ v else v ++ h // mind the gap
  moves ++ "A"
}.ensuring(res => isAllowed(dirPad)(res, from))

def dirs2dirs(dirs: String, state: Char): (String, Char) =
  val ds = f"$state$dirs".zip(dirs).flatMap(dir2dirs)
  (ds.mkString, ds.last)

def encodeChar(
    c: Char,
    numPadState: Char,
    keyPad1State: Char,
    keyPad2State: Char
): (String, Char, Char, Char) =
  val paths = num2dirs(numPadState, c)
  paths
    .map(d0 =>
      val (d1, kp1) = dirs2dirs(d0, keyPad1State)
      val (d2, kp2) = dirs2dirs(d1, keyPad2State)
      (d2, c, kp1, kp2)
    )
    .minBy((d, _, _, _) => d.size) // take path with the shortest final input

def encodeCode(
    code: String,
    numPadState: Char,
    keyPad1State: Char,
    keyPad2State: Char
): (String, Char, Char, Char) =
  code
    .foldLeft(("", numPadState, keyPad1State, keyPad2State))((acc, c) =>
      val (dIn, nIn, k1In, k2In) = acc
      val (dOut, nOut, k1Out, k2Out) = encodeChar(c, nIn, k1In, k2In)
      (dIn + dOut, nOut, k1Out, k2Out)
    )

def encodeCodes(codes: List[String]): List[String] =
  codes
    .foldLeft((List[String](), 'A', 'A', 'A'))((acc, c) =>
      val (dIn, nIn, k1In, k2In) = acc
      val (dOut, nOut, k1Out, k2Out) = encodeCode(c, nIn, k1In, k2In)
      (dIn :+ dOut, nOut, k1Out, k2Out)
    )
    ._1

def complexity(code: String, directions: String) =
  code.replace("A", "").toLong * directions.size

def solve1: Long = input.zip(encodeCodes(input.toList)).map(complexity).sum
def solve2: Int = ???
