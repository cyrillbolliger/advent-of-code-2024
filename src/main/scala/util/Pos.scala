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
