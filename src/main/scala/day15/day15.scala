package me.cyrill.aoc2024.day15

import scala.io.Source
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*

val inputPath = "src/main/scala/day15/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

def parseMoves(input: Array[String]): Array[Char] = {
  input.dropWhile(_ != "").reduce(_ + _).trim.toArray
}.ensuring(_.forall("v^<>".contains(_)))

case class Warehouse(
    private val data: Array[Char],
    val width: Int,
    val height: Int,
    private val bot: Int
) extends BaseMatrix[Char](data, width, height):

  def this(input: Array[Array[Char]]) =
    this(
      input.flatten,
      input(0).size,
      input.size,
      input.flatten.indexOf('@')
    )

  def updated(pos: Pos, value: Char): Warehouse =
    throwIfOutOfBounds(pos)
    this.copy(
      data = data.updated(toIdx(pos), value),
      bot = if value == '@' then toIdx(pos) else bot
    )

  lazy val botPos: Pos = toPos(bot)

  def boxCoords(box: Char): Array[Int] =
    data.zipWithIndex
      .filter(_._1 == box)
      .map((_, idx) =>
        val (x, y) = toPos(idx)
        x + y * 100
      )

def moveRecursive(
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
    case c =>
      moveRecursive(state, next, c, nextPos)
        .flatMap(moveRecursive(_, pos, char, nextPos))

def solve1: Int = challenge1.solve(input)
def solve2: Int = challenge2.solve(input)
