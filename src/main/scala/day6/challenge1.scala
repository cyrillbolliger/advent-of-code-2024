package me.cyrill.aoc2024.day6.challenge1

import me.cyrill.aoc2024.day6.*
import Field.*
import Orientation.*

import scala.io.Source

def step(map: AMap): Boolean =
  val currentPos = map.getGuardPos
  val nextPos = getNextPos(currentPos, map.getGuardOrientation)

  if map.onMap(nextPos) then
    val next = map(nextPos)
    next match
      case Unknown | Inspected(_) =>
        map.update(nextPos)(Guard(map.getGuardOrientation))
      case Obstacle | Obstruction =>
        map.update(currentPos)(Guard(map.getGuardOrientation).turn)
      case Guard(d) => throw Exception(f"Illegal guard position")
    true
  else false

def inspect(map: AMap): Unit = while step(map) do ()

def solve(input: Array[String]) =
  val m = AMap(input)
  inspect(m)
  m.inspected + 1
