package me.cyrill.aoc2024.day6

import me.cyrill.aoc2024.day6.Field.*
import me.cyrill.aoc2024.day6.Orientation.*

class AMap(definition: Array[String]):
  require(!definition.isEmpty)
  require(
    definition.map(_.length).max == definition.map(_.length).min,
    "Invalid definition: All rows must have the same length."
  )

  private val colCount: Int = definition(0).length
  private val rowCount: Int = definition.length
  private val data: Array[Field] = parse(definition)

  private def toPos(idx: Int) = (idx % colCount, idx / colCount)
  private def toIdx(pos: (Int, Int)) =
    val (x, y) = pos: (Int, Int)
    if onMap(pos) then x + colCount * y
    else
      throw IllegalArgumentException(
        f"x or y if off the map. x: $x, y: $y, columns: $colCount, rows: $rowCount"
      )

  private var guardPos: (Int, Int) =
    toPos:
      data.indexWhere(f =>
        f match
          case Guard(d) => true
          case _        => false
      )

  private def parse(d: Array[String]): Array[Field] =
    d.map(_.toArray)
      .flatten
      .map(s =>
        s match
          case '#' => Obstacle
          case '.' => Unknown
          case 'X' => Inspected
          case '^' => Guard(Up)
          case '>' => Guard(Right)
          case 'v' => Guard(Down)
          case '<' => Guard(Left)
          case _ =>
            throw IllegalArgumentException(f"Unknown char in definition: $s")
      )

  def apply(pos: (Int, Int)): Field =
    val f = data(toIdx(pos))
    f // copy to val first to make it immutable

  def update(pos: (Int, Int))(value: Field): Unit =
    // update map
    data(toIdx(pos)) = value

    // set guard pos
    value match
      case Guard(d) => guardPos = pos
      case _        => ()

  def onMap(pos: (Int, Int)): Boolean =
    val (x, y) = pos: (Int, Int)
    x >= 0 && x < colCount && y >= 0 && y < rowCount

  def getGuardPos =
    val p = guardPos
    p // copy to val first to make it immutable

  def inspected: Int =
    data.count(_ == Inspected)

  override def toString: String =
    data
      .foldLeft("")((acc, f) =>
        acc + (f match
          case Obstacle     => '#'
          case Unknown      => '.'
          case Inspected    => 'X'
          case Guard(Up)    => '^'
          case Guard(Right) => '>'
          case Guard(Down)  => 'v'
          case Guard(Left)  => '<'
        )
      )
      .grouped(colCount)
      .foldLeft("")((acc, row) => acc + row + "\n")
