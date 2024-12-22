package me.cyrill.aoc2024.day16

import scala.io.Source
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*
import scala.annotation.tailrec
import me.cyrill.aoc2024.day16.Cell.Start
import me.cyrill.aoc2024.day16.Cell.End
import me.cyrill.aoc2024.day16.Cell.Wall
import me.cyrill.aoc2024.day16.Cell.Path
import me.cyrill.aoc2024.day16.Cell.Free
import me.cyrill.aoc2024.day8.onMap

val inputPath = "src/main/scala/day16/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

enum Dir:
  case North
  case East
  case South
  case West

  def toPos(p: Pos): Pos =
    this match
      case North => p.north
      case East  => p.east
      case South => p.south
      case West  => p.west

  def opposite: Dir =
    this match
      case North => South
      case East  => West
      case South => North
      case West  => East

sealed trait Cell:
  def costs: Option[Int] = this match
    case Cell.Path(costs) => Some(costs)
    case Cell.End(costs)  => Some(costs)
    case _                => None

  def isEnd: Boolean = this match
    case Cell.End(_) => true
    case _           => false

  override def toString(): String = this match
    case Start   => "SSSSS "
    case End(_)  => "EEEEE "
    case Wall    => "##### "
    case Path(c) => f"$c%05d "
    case Free    => "..... "

object Cell:
  case object Start extends Cell
  case class End(c: Int) extends Cell
  case object Wall extends Cell
  case class Path(c: Int) extends Cell
  case object Free extends Cell

case class Maze(
    private val data: Vector[Cell],
    val width: Int,
    val height: Int,
    private val start: Int,
    val orientation: Dir,
    val toExplore: Map[Pos, Dir],
    val scores: Set[Int],
    val done: Boolean
) extends BaseMatrix[Cell, Vector](data, width, height):

  def this(input: Vector[Vector[Cell]]) =
    this(
      input.flatten.toVector,
      input(0).size,
      input.size,
      input.flatten.indexOf(Cell.Start),
      Dir.East,
      Map(),
      Set(),
      false
    )

  def updated(pos: Pos, value: Cell): Maze =
    throwIfOutOfBounds(pos)
    this.copy(
      data = data.updated(toIdx(pos), value)
    )

  val startPos: Pos = toPos(start)

  lazy val currentCosts = apply(startPos).costs.getOrElse(0)

  def costs(d: Dir) =
    if d == orientation.opposite then
      throw IllegalArgumentException(
        f"Costs of opposite direction are not defined"
      )
    else if d == orientation then 1
    else 1001

  lazy val possibleSteps: List[(Dir, Pos, Int, Cell)] =
    Dir.values.toList
      .filter(_ != orientation.opposite)
      .map(d => (d, d.toPos(startPos)))
      .filter((_, p) => hasPos(p))
      .map((d, p) => (d, p, currentCosts + costs(d), apply(p)))
      .filter((_, pos, costs, cell) =>
        cell match
          case Cell.End(_) | Cell.Free => true
          case Cell.Start | Cell.Wall  => false
          case Cell.Path(c)            => c > costs
      )

  def step: Maze =
    if possibleSteps.isEmpty then
      if toExplore.isEmpty then
        // Done
        if scores.nonEmpty then this.copy(done = true)
        else throw Exception("Failed to find path")
      else
        // Further paths to explore
        val (s, o) = toExplore.head
        this.copy(start = toIdx(s), orientation = o, toExplore = toExplore.tail)
    else
      // Continue path and add possible further paths to toExplore list
      val (dir, pos, costs, cell) = possibleSteps.head

      val explore =
        if toExplore.get(pos).isDefined then toExplore.updated(pos, dir)
        else toExplore

      val newCell = if cell.isEnd then Cell.End(costs) else Cell.Path(costs)

      this.copy(
        data = data.updated(toIdx(pos), newCell),
        orientation = dir,
        start = toIdx(pos),
        scores = if cell.isEnd then scores + costs else scores,
        toExplore =
          if possibleSteps.tail.isEmpty then explore
          else explore + (toPos(start) -> orientation)
      )

def parse(input: Array[String]): Maze =
  new Maze(
    input
      .map(
        _.toVector.map(c =>
          c match
            case 'S' => Cell.Start
            case 'E' => Cell.End(Int.MaxValue)
            case '#' => Cell.Wall
            case '.' => Cell.Free
            case _ =>
              throw IllegalArgumentException(f"Failed to parse cell: $c")
        )
      )
      .toVector
  )

@tailrec
def solve(m: Maze): Int =
  val next = m.step
  if next.done then m.scores.min
  else solve(next)

def solve1: Int = solve(parse(input))
def solve2: Int = ???
