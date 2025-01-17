package me.cyrill.aoc2024.util.pos

type X = Int
type Y = Int
type Pos = (X, Y)

extension (pos: Pos)
  def north: Pos = (pos._1, pos._2 - 1)
  def east: Pos = (pos._1 + 1, pos._2)
  def south: Pos = (pos._1, pos._2 + 1)
  def west: Pos = (pos._1 - 1, pos._2)

  def northWest: Pos = (pos._1 - 1, pos._2 - 1)
  def northEast: Pos = (pos._1 + 1, pos._2 - 1)
  def southWest: Pos = (pos._1 - 1, pos._2 + 1)
  def southEast: Pos = (pos._1 + 1, pos._2 + 1)

  def adjecent: Set[Pos] = Set(pos.north, pos.east, pos.south, pos.west)
  def surrounding: Set[Pos] =
    adjecent ++ Set(pos.northEast, pos.northWest, pos.southEast, pos.southWest)

  def capped(cap: Int) = (Math.min(pos._1, cap), Math.min(pos._2, cap))

  def manhattanDist(p: Pos) =
    val (dx, dy) = p - pos
    Math.abs(dx) + Math.abs(dy)

  def topLeft(p: Pos*) =
    val x = (pos +: p).map(_._1).min
    val y = (pos +: p).map(_._2).min
    (x, y)

  def bottomRight(p: Pos*) =
    val x = (pos +: p).map(_._1).max
    val y = (pos +: p).map(_._2).max
    (x, y)

  def delta(p: Pos) = (Math.abs(pos._1 - p._1), Math.abs(pos._2 - p._2))

  infix def /(p: Pos) = (pos._1 / p._1, pos._2 / p._2)
  infix def /(n: Int) = (pos._1 / n, pos._2 / n)

  infix def *(p: Pos) = (pos._1 * p._1, pos._2 * p._2)
  infix def *(n: Int) = (pos._1 * n, pos._2 * n)

  infix def +(p: Pos) = (pos._1 + p._1, pos._2 + p._2)
  infix def -(p: Pos) = (pos._1 - p._1, pos._2 - p._2)

  infix def %(p: Pos) = (pos._1 % p._1, pos._2 % p._2)
