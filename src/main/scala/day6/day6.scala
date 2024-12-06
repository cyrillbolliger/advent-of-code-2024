package me.cyrill.aoc2024.day6

import Field.*
import Direction.*
import scala.io.Source

val inputPath = "src/main/scala/day6/input.txt"
lazy val input = Source.fromFile(inputPath).getLines().toArray

def step(map: AMap): Boolean =
  val currentPos = map.getGuardPos

  val guard: Guard = map(currentPos) match
    case Guard(d) => Guard(d)
    case _        => throw Exception(f"Illegal guard position")

  val nextPos = guard match
    case Guard(Up)    => (currentPos._1, currentPos._2 - 1)
    case Guard(Right) => (currentPos._1 + 1, currentPos._2)
    case Guard(Down)  => (currentPos._1, currentPos._2 + 1)
    case Guard(Left)  => (currentPos._1 - 1, currentPos._2)
    case _            => throw Exception(f"Illegal guard position")

  if map.onMap(nextPos) then
    val next = map(nextPos)
    next match
      case Unknown | Inspected =>
        map.update(currentPos)(Inspected)
        map.update(nextPos)(guard)
      case Obstacle =>
        map.update(currentPos)(guard.turn)
      case Guard(d) => throw Exception(f"Illegal guard position")
    true
  else false

def inspect(map: AMap): Unit = while step(map) do ()

def solve1 =
  val m = AMap(input)
  inspect(m)
  m.inspected + 1
