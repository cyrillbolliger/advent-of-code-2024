package me.cyrill.aoc2024.day15.challenge1

import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*
import me.cyrill.aoc2024.day15.{parseMoves, moveRecursive, Warehouse}

def parseWarehouse(input: Array[String]): Warehouse =
  new Warehouse(input.takeWhile(_ != "").map(_.toArray))

def move(state: Warehouse, mv: Char): Option[Warehouse] =
  mv match
    case '^' => moveRecursive(state, state.botPos, '@', _.north)
    case '>' => moveRecursive(state, state.botPos, '@', _.east)
    case 'v' => moveRecursive(state, state.botPos, '@', _.south)
    case '<' => moveRecursive(state, state.botPos, '@', _.west)
    case _   => throw IllegalArgumentException(f"Unknown move: $mv")

def solve(input: Array[String]): Int =
  val state = parseWarehouse(input)
  val moves = parseMoves(input)
  moves.foldLeft(state)((s, m) => move(s, m).getOrElse(s)).boxCoords('O').sum
