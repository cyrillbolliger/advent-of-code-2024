package me.cyrill.aoc2024.day6.challenge2

import me.cyrill.aoc2024.day6.*
import Field.*
import Orientation.*
import Direction.*

import scala.io.Source

enum Inspection:
  case Ongoing
  case OffMap
  case Trapped

def isBlocked(map: AMap, pos: (Int, Int)): Boolean =
  if map.onMap(pos) then map(pos).isBlocked
  else false

def goToNext(
    map: AMap,
    nextPos: (Int, Int),
    orientation: Orientation
): Inspection =
  if map.onMap(nextPos) then
    val currentPos = map.getGuardPos
    val next = map(nextPos)
    val afterNextIsBlocked = isBlocked(map, getNextPos(nextPos, orientation))

    next match
      case Inspected(Both) if afterNextIsBlocked => Inspection.Trapped
      case Inspected(d) =>
        map.update(nextPos)(Guard(orientation))
        Inspection.Ongoing
      case Unknown =>
        map.update(nextPos)(Guard(orientation))
        Inspection.Ongoing
      case Obstacle | Obstruction =>
        val initialOrientation = map.getGuardOrientation
        val newOrientation = Guard(orientation).turn.getOrientation.get
        if newOrientation == initialOrientation then Inspection.Trapped
        else
          val newPos = getNextPos(currentPos, newOrientation)
          val res = goToNext(map, newPos, newOrientation)
          map.update(currentPos)(Inspected(Both))
          res
      case Guard(d) => throw Exception(f"Illegal guard position")
  else Inspection.OffMap

def step(map: AMap): Inspection =
  val currentPos = map.getGuardPos
  val guardOrientation = map.getGuardOrientation
  val nextPos = getNextPos(currentPos, guardOrientation)
  goToNext(map, nextPos, guardOrientation)

def inspect(map: AMap): Inspection =
  var res = step(map)
  while res == Inspection.Ongoing do res = step(map)
  res

def solve(input: Array[String]) =
  var positions = 0
  // TODO: optimization:
  //       only iterate over the fields the guard can check,
  //       reuse the the logic from challenge 1 to determine the fields
  for
    x <- 0 until input(0).length
    y <- 0 until input.length
  do
    val m = AMap(input)
    if m(x, y) == Unknown then
      m.update((x, y))(Obstruction)
      val res = inspect(m)
      if res == Inspection.Trapped then positions += 1
  positions
