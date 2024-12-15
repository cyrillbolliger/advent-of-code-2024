package me.cyrill.aoc2024.day12

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

val inputPath = "src/main/scala/day12/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray
def parse(input: Array[String]): GMap =
  GMap(input.map(row => row.map(Plot(_)).toArray))

type X = Int
type Y = Int
type Pos = (X, Y)

extension (pos: Pos)
  def north: Pos = (pos._1, pos._2 - 1)
  def east: Pos = (pos._1 + 1, pos._2)
  def south: Pos = (pos._1, pos._2 + 1)
  def west: Pos = (pos._1 - 1, pos._2)

case class Plot(val name: Char):
  override def toString(): String = name.toString

case class GMap(val data: Array[Array[Plot]]):
  require(data.size > 0)

  val width: Int = data(0).size
  val height: Int = data.size

  def apply(pos: Pos): Plot = data(pos._2)(pos._1)

  def neighbours(pos: Pos): Set[Pos] =
    val plot = apply(pos)
    Set(pos.north, pos.east, pos.south, pos.west)
      .filter((x, y) =>
        x >= 0 && x < width && y >= 0 && y < height
          && this((x, y)) == plot
      )

  lazy val positions = for
    y <- 0 until height
    x <- 0 until width
  yield (x, y)

  override def toString(): String =
    positions
      .map(apply)
      .grouped(width)
      .map(_.foldLeft("")(_.toString + _.toString))
      .reduce(_ + "\n" + _)

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

case class Region(positions: Set[Pos]):
  def area: Int = positions.size

  def neighbours(pos: Pos): Set[Pos] =
    Set(pos.north, pos.east, pos.south, pos.west).intersect(positions)

  def perimeter: Int =
    positions.toList
      .map(p => 4 - neighbours(p).size)
      .sum

  def sides: Int = ???

  def price = area * perimeter

def solve1(input: Array[String]): Int =
  val gmap = parse(input)
  val garden = Garden(gmap)

  var toProcess = gmap.positions.toSet
  var price: Int = 0

  while toProcess.nonEmpty do
    val region = garden.findRegion(toProcess.head)
    if region.nonEmpty then
      toProcess = toProcess -- region
      price += Region(region).price

  price

def solve1: Int = solve1(input)
