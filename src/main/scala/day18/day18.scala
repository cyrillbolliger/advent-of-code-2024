package me.cyrill.aoc2024.day18

import scala.io.Source
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*
import scala.collection.mutable.PriorityQueue
import scala.util.boundary, boundary.break

val inputPath = "src/main/scala/day18/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

case class Node(
    val pos: Pos,
    val costFromStart: Int,
    val estimateToEnd: Int,
    val parent: Option[Node]
) extends Ordered[Node]:
  val estimateTotal = costFromStart + estimateToEnd

  lazy val toStart: List[Node] = parent match
    case None    => this :: Nil
    case Some(p) => this :: p.toStart

  override def compare(that: Node): Int =
    that.estimateTotal.compare(this.estimateTotal)
end Node

def makeMap(walls: Set[Pos], width: Int, height: Int): Matrix[Char, Vector] =
  new Matrix(
    Vector.tabulate(height, width)((y, x) =>
      if walls.contains((x, y)) then '#' else '.'
    )
  )

def estimate(a: Pos, b: Pos): Int =
  val dx = Math.abs(a._1 - b._1)
  val dy = Math.abs(a._2 - b._2)
  dx + dy

def shortestPath(
    m: Matrix[Char, Vector]
)(startPos: Pos, endPos: Pos): List[Node] =
  val start = Node(startPos, 0, estimate(startPos, endPos), None)
  val open = PriorityQueue[Node](start)
  val closed = scala.collection.mutable.Set[Pos]()

  boundary:
    while open.nonEmpty do
      val current = open.dequeue()

      if current.pos == endPos then
        // we've reached the end
        break(current.toStart)
      else
        closed.addOne(current.pos)

        val candidatePos = m
          .adjecentWithPos(current.pos)
          .filter((p, c) => c != '#' && !closed.contains(p))
          .map(_._1)

        for pos <- candidatePos do
          val costFromStart = current.costFromStart + 1
          val betterExists =
            open.exists(n => n.pos == pos && n.costFromStart <= costFromStart)
          if (!betterExists) then
            open.enqueue(
              Node(pos, costFromStart, estimate(pos, endPos), Some(current))
            )
    List()

def parse(input: Array[String]): Set[Pos] =
  input.map(_.split(",")).map(a => (a(0).toInt, a(1).toInt)).toSet

def solve1(
    input: Array[String],
    width: Int,
    height: Int,
    startPos: Pos,
    endPos: Pos
): Int =
  val corruped = parse(input)
  val m = makeMap(corruped, width, width)
  shortestPath(m)(startPos, endPos).size - 1

def solve2(
    input: Array[String],
    width: Int,
    height: Int,
    startPos: Pos,
    endPos: Pos,
    minWalls: Int
): String =
  boundary:
    for i <- minWalls until input.size do
      val corruped = parse(input.take(i))
      val m = makeMap(corruped, width, width)
      if shortestPath(m)(startPos, endPos) == Nil then break(input(i - 1))
    throw Exception("Path found after adding all walls")

def solve1: Int = solve1(input.take(1024), 71, 71, (0, 0), (70, 70))
def solve2: String = solve2(input, 71, 71, (0, 0), (70, 70), 1024)
