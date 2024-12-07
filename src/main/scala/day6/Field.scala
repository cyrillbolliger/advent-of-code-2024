package me.cyrill.aoc2024.day6

enum Orientation:
  case Up
  case Down
  case Left
  case Right

  def toDirection: Direction =
    if this == Left || this == Right
    then Direction.Horizontal
    else Direction.Vertical

enum Direction:
  case Horizontal
  case Vertical
  case Both

enum Field:
  case Obstacle
  case Unknown
  case Inspected(d: Direction)
  case Guard(o: Orientation)
  case Obstruction

  def getOrientation: Option[Orientation] =
    this match
      case Guard(o) => Some(o)
      case _        => None

  def getDirection: Option[Direction] =
    this match
      case Inspected(d) => Some(d)
      case _            => None

  def isBlocked: Boolean = this == Obstacle || this == Obstruction

import Field.*
import Orientation.*

extension (guard: Guard)
  def turn =
    guard match
      case Guard(Up)    => Guard(Right)
      case Guard(Right) => Guard(Down)
      case Guard(Down)  => Guard(Left)
      case Guard(Left)  => Guard(Up)
