package me.cyrill.aoc2024.day12

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*

val inputPath = "src/main/scala/day12/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray
def parse(input: Array[String]): GMap =
  GMap(input.map(row => row.map(Plot(_)).toVector).toVector)

case class Plot(val name: Char):
  override def toString(): String = name.toString

class GMap(data: Vector[Vector[Plot]]) extends Matrix[Plot, Vector](data):
  def neighbours(pos: Pos): Set[Pos] =
    val plot = this(pos)
    pos.adjecent.filter(p => this.hasPos(p) && this(p) == plot)

case class Garden(gmap: GMap):
  @tailrec
  private def findRegion(
      pos: Set[Pos],
      acc: Set[Pos]
  ): Set[Pos] =
    val newNeighbours = pos.flatMap(gmap.neighbours(_)) -- acc
    if newNeighbours.isEmpty then acc
    else findRegion(newNeighbours, acc ++ newNeighbours)

  def findRegion(pos: Pos): Set[Pos] = findRegion(Set(pos), Set(pos))

case class EdgeSet(edges: Set[Pos], vertical: Boolean):
  def neighbours(pos: Pos): Set[Pos] =
    val candidates =
      if vertical then Set(pos.north, pos.south)
      else Set(pos.east, pos.west)
    candidates.intersect(edges)

  @tailrec
  private def findEdge(
      pos: Set[Pos],
      acc: Set[Pos]
  ): Set[Pos] =
    val newNeighbours = pos.flatMap(neighbours(_)) -- acc
    if newNeighbours.isEmpty then acc
    else findEdge(newNeighbours, acc ++ newNeighbours)

  def findEdge(pos: Pos): Set[Pos] = findEdge(Set(pos), Set(pos))

  def findEdges: Set[Set[Pos]] =
    var toProcess = edges
    var found = scala.collection.mutable.Set[Set[Pos]]()

    while toProcess.nonEmpty do
      val edge = this.findEdge(toProcess.head)
      if edge.nonEmpty then toProcess = toProcess -- edge
      found.addOne(edge)

    found.toSet

case class Region(positions: Set[Pos]):
  def area: Int = positions.size

  def neighbours(pos: Pos): Set[Pos] =
    Set(pos.north, pos.east, pos.south, pos.west).intersect(positions)

  def perimeter: Int =
    positions.toList
      .map(p => 4 - neighbours(p).size)
      .sum

  def edges(
      pos: Pos,
      acc: (Set[Pos], Set[Pos], Set[Pos], Set[Pos])
  ): (Set[Pos], Set[Pos], Set[Pos], Set[Pos]) =
    val (n, e, s, w) = acc
    val north = if positions.exists(_ == pos.north) then n else n + pos
    val east = if positions.exists(_ == pos.east) then e else e + pos
    val south = if positions.exists(_ == pos.south) then s else s + pos
    val west = if positions.exists(_ == pos.west) then w else w + pos
    (north, east, south, west)

  def sides: Int =
    val (n, e, s, w) =
      positions.foldLeft((Set[Pos](), Set[Pos](), Set[Pos](), Set[Pos]()))(
        (acc, pos) => edges(pos, acc)
      )

    EdgeSet(n, false).findEdges.size
      + EdgeSet(e, true).findEdges.size
      + EdgeSet(s, false).findEdges.size
      + EdgeSet(w, true).findEdges.size

  def price = area * perimeter
  def discountPrice = area * sides

def solve(input: Array[String], priceFn: (Region => Int)): Int =
  val gmap = parse(input)
  val garden = Garden(gmap)

  var toProcess = gmap.positions.toSet
  var price: Int = 0

  while toProcess.nonEmpty do
    val region = garden.findRegion(toProcess.head)
    if region.nonEmpty then
      toProcess = toProcess -- region
      price += priceFn(Region(region))

  price

def solve1: Int = solve(input, _.price)
def solve2: Int = solve(input, _.discountPrice)
