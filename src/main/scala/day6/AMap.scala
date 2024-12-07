package me.cyrill.aoc2024.day6

import me.cyrill.aoc2024.day6.Field.*
import me.cyrill.aoc2024.day6.Orientation.*
import me.cyrill.aoc2024.day6.Direction.*

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
      definition
        .map(_.toArray)
        .flatten
        .indexWhere(f =>
          f match
            case '>' | '<' | '^' | 'v' => true
            case _                     => false
        )

  private var guardOrientation: Orientation =
    val input = definition
      .map(_.toArray)
      .flatten
    input(toIdx(guardPos)) match
      case '^' => Up
      case '>' => Right
      case 'v' => Down
      case '<' => Left
      case _ =>
        throw Exception(f"Expected guard, found ${data(toIdx(guardPos))}")

  private def parse(d: Array[String]): Array[Field] =
    d.map(_.toArray)
      .flatten
      .map(s =>
        s match
          case '#'             => Obstacle
          case '.'             => Unknown
          case '-' | '>' | '<' => Inspected(Horizontal)
          case '|' | '^' | 'v' => Inspected(Vertical)
          case '+'             => Inspected(Both)
          case 'O'             => Obstruction
          case _ =>
            throw IllegalArgumentException(f"Unknown char in definition: $s")
      )

  def apply(pos: (Int, Int)): Field =
    val f = data(toIdx(pos))
    f // copy to val first to make it immutable

  def update(pos: (Int, Int))(value: Field): Unit =
    value match
      case Guard(o) =>
        // set guard pos
        guardPos = pos
        guardOrientation = o
        // and mark the field as inspected
        val d = data(toIdx(pos)).getDirection.getOrElse(o.toDirection)
        data(toIdx(pos)) = Inspected(if d == o.toDirection then d else Both)
      case _ => data(toIdx(pos)) = value

  def onMap(pos: (Int, Int)): Boolean =
    val (x, y) = pos: (Int, Int)
    x >= 0 && x < colCount && y >= 0 && y < rowCount

  def getGuardPos =
    val p = guardPos
    p // copy to val first to make it immutable

  def getGuardOrientation =
    val o = guardOrientation
    o // copy to val first to make it immutable

  def inspected: Int =
    data.count(_ match
      case Inspected(_) => true
      case _            => false
    )

  override def toString: String =
    val d = data.clone
    d(toIdx(guardPos)) = Guard(guardOrientation)
    d
      .foldLeft("")((acc, f) =>
        acc + (f match
          case Obstacle              => '#'
          case Unknown               => '.'
          case Inspected(Horizontal) => '-'
          case Inspected(Vertical)   => '|'
          case Inspected(Both)       => '+'
          case Guard(Up)             => '^'
          case Guard(Right)          => '>'
          case Guard(Down)           => 'v'
          case Guard(Left)           => '<'
          case Obstruction           => 'O'
        )
      )
      .grouped(colCount)
      .foldLeft("")((acc, row) => acc + row + "\n")
