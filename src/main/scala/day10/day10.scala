package me.cyrill.aoc2024.day10

import scala.io.Source

val inputPath = "src/main/scala/day10/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

def parse(in: Array[String]): TMap =
  in.map(_.map(_.toString.toInt).toVector).toVector

type Path = List[Coord]

type TMap = Vector[Vector[Int]]

extension (m: TMap)
  private def onMap(coord: Coord) =
    coord.y >= 0 && coord.y < m.size
      && coord.x >= 0 && coord.x < m(coord.y).length

  private def altitude(coord: Coord): Option[Int] =
    m.onMap(coord) match
      case true  => Some(m(coord.y)(coord.x))
      case false => None

  def get(coord: Coord): Option[Cell] =
    altitude(coord) match
      case Some(altitude) => Some(Cell(altitude, coord)(m))
      case _              => None

  def paths: Set[Path] =
    (for
      y <- 0 until m.length
      x <- 0 until m(y).length
      coord = Coord(x, y)
      if m.altitude(coord) == Some(9)
      node = Node(m.get(coord).get)
    yield node.paths).flatten.toSet

case class Coord(x: Int, y: Int):
  lazy val neighbours = Neighbours(
    Coord(x, y - 1),
    Coord(x + 1, y),
    Coord(x, y + 1),
    Coord(x - 1, y)
  )

case class Cell(altitude: Int, coord: Coord)(m: => TMap):
  lazy val neighbours = coord.neighbours.map(m.get)

case class Neighbours[T](
    n: T,
    w: T,
    s: T,
    e: T
):
  def map[B](f: T => B): Neighbours[B] =
    Neighbours[B](f(n), f(w), f(s), f(e))

  def toSet: Set[T] = Set(n, w, s, e)

case class Node(cell: Cell):
  lazy val neighbours =
    cell.neighbours.map(
      _ match
        case Some(c) if cell.altitude - 1 == c.altitude => Some(Node(c))
        case _                                          => None
    )

  lazy val paths: Set[Path] =
    neighbours
      .map(
        _ match
          case None => None
          case Some(n) if n.cell.altitude == 0 =>
            Some(Set(List(n.cell.coord, cell.coord)))
          case Some(n) =>
            val childPaths = n.paths.map(p => p :+ cell.coord)
            if childPaths.isEmpty then None else Some(childPaths)
      )
      .toSet
      .filter(_.isDefined)
      .flatMap(_.get)

def solve1: Int = solve1(input)
def solve1(input: Array[String]): Int =
  parse(input).paths.map(p => (p.head, p.last)).size

def solve2: Int = solve2(input)
def solve2(input: Array[String]): Int = parse(input).paths.size
