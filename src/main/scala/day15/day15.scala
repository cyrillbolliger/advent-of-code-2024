package me.cyrill.aoc2024.day15

import scala.io.Source
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*

// for the sake of efficiency, this challange violates greatly the
// principles of functional programming. all state updates are
// side effects.

val inputPath = "src/main/scala/day15/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

class Warehouse(input: Array[Array[Char]]) extends Matrix[Char](input):
  var bot = toPos(data.indexOf('@'))
  override def update(pos: Pos, value: Char): Unit =
    super.update(pos, value)
    if value == '@' then bot = pos

  def boxCoords: Array[Int] =
    data.zipWithIndex
      .filter(_._1 == 'O')
      .map((_, idx) =>
        val (x, y) = toPos(idx)
        x + y * 100
      )

def parseWarehouse(input: Array[String]): Warehouse =
  Warehouse(input.takeWhile(_ != "").map(_.toArray))

def parseMoves(input: Array[String]): Array[Char] = {
  input.dropWhile(_ != "").reduce(_ + _).trim.toArray
}.ensuring(_.forall("v^<>".contains(_)))

def move(
    state: Warehouse,
    pos: Pos,
    char: Char,
    nextPos: Pos => Pos
): Boolean =
  val next = nextPos(pos)
  state(next) match
    case '#' => false
    case '.' =>
      state(next) = char
      state(pos) = '.'
      true
    case 'O' =>
      move(state, next, 'O', nextPos)
      && move(state, pos, char, nextPos)
    case _ => throw IllegalStateException(f"Unknown state: ${state(next)}")

def move(state: Warehouse, mv: Char): Unit =
  mv match
    case '^' => move(state, state.bot, '@', _.north)
    case '>' => move(state, state.bot, '@', _.east)
    case 'v' => move(state, state.bot, '@', _.south)
    case '<' => move(state, state.bot, '@', _.west)
    case _   => throw IllegalArgumentException(f"Unknown move: $mv")

def solve(input: Array[String]): Int =
  val state = parseWarehouse(input)
  val moves = parseMoves(input)
  moves.foreach(move(state, _))
  state.boxCoords.sum

def solve1: Int = solve(input)
def solve2: Int = ???
