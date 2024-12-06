package me.cyrill.aoc2024.day6

enum Orientation:
  case Up
  case Down
  case Left
  case Right

enum Field:
  case Obstacle
  case Unknown
  case Inspected
  case Guard(d: Orientation)

import Field.*
import Orientation.*

extension (guard: Guard)
  def turn =
    guard match
      case Guard(Up)    => Guard(Right)
      case Guard(Right) => Guard(Down)
      case Guard(Down)  => Guard(Left)
      case Guard(Left)  => Guard(Up)
