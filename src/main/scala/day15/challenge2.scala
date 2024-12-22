package me.cyrill.aoc2024.day15.challenge2

import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*
import me.cyrill.aoc2024.day15.{parseMoves, moveRecursive, Warehouse}

def parseWarehouse(input: Array[String]): Warehouse =
  val data = input
    .takeWhile(_ != "")
    .map(_.toVector.flatMap(_ match
      case '#' => Array('#', '#')
      case 'O' => Array('[', ']')
      case '.' => Array('.', '.')
      case '@' => Array('@', '.')
      case c   => throw IllegalArgumentException(f"Unknown map char: $c")
    ))
    .toVector

  new Warehouse(data)

def moveRecVertical(
    state: Warehouse,
    pos: Pos,
    char: Char,
    nextPos: Pos => Pos
): Option[Warehouse] =
  val next = nextPos(pos)
  state(next) match
    case '#' => None
    case '.' =>
      Some(state.updated(next, char).updated(pos, '.'))
    case '[' =>
      moveRecVertical(state, next.east, ']', nextPos)
        .flatMap(moveRecVertical(_, next, '[', nextPos))
        .flatMap(moveRecVertical(_, pos, char, nextPos))
    case ']' =>
      moveRecVertical(state, next.west, '[', nextPos)
        .flatMap(moveRecVertical(_, next, ']', nextPos))
        .flatMap(moveRecVertical(_, pos, char, nextPos))

def move(state: Warehouse, mv: Char): Option[Warehouse] =
  mv match
    case '^' => moveRecVertical(state, state.botPos, '@', _.north)
    case '>' => moveRecursive(state, state.botPos, '@', _.east)
    case 'v' => moveRecVertical(state, state.botPos, '@', _.south)
    case '<' => moveRecursive(state, state.botPos, '@', _.west)
    case _   => throw IllegalArgumentException(f"Unknown move: $mv")

def solve(input: Array[String]): Int =
  val state = parseWarehouse(input)
  val moves = parseMoves(input)
  moves.foldLeft(state)((s, m) => move(s, m).getOrElse(s)).boxCoords('[').sum
