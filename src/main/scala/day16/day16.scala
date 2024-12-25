package me.cyrill.aoc2024.day16

import scala.io.Source
import me.cyrill.aoc2024.util.pos.*
import me.cyrill.aoc2024.util.matrix.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.util.boundary, boundary.break
import scala.compiletime.ops.double

val COST_NEXT = 1
val COST_TURN = 1000

val inputPath = "src/main/scala/day16/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

enum Dir:
  case North
  case East
  case South
  case West

  def opposite: Dir =
    this match
      case North => South
      case East  => West
      case South => North
      case West  => East
end Dir

class Maze(input: Vector[Vector[Char]]) extends Matrix[Char, Vector](input):
  lazy val startPos = toPos(data.indexOf('S'))
  lazy val endPos = toPos(data.indexOf('E'))

  def withoutDeadEnds: Maze =
    val blocked = scala.collection.mutable.Set[Pos]()
    var deadEndFound = true

    while deadEndFound do
      val dead = for
        pos <- positions
        if !blocked.contains(pos)
        if apply(pos) == '.'
        n = pos.adjecent.toList
          .filter(p => !blocked.contains(p) && hasPos(p))
          .map(apply)
          .filter(_ != '#')
        if n.size == 1
        if n.head == '.'
      yield pos
      if dead.size == 0 then deadEndFound = false
      else blocked.addAll(dead)

    Maze(
      data.zipWithIndex
        .map((c, idx) => if blocked.contains(toPos(idx)) then '#' else c)
        .toVector
        .grouped(width)
        .toVector
    )
end Maze

case class Node(
    val pos: Pos,
    val direction: Dir,
    val costFromStart: Int,
    val estimateToEnd: Int,
    val parent: Option[Node],
    val alternatives: Set[Pos]
):
  val estimateTotal = costFromStart + estimateToEnd

  lazy val toStart: List[Node] = parent match
    case None    => this :: Nil
    case Some(p) => this :: p.toStart

  def costTo(p: Pos): Int =
    require(pos.adjecent.contains(p), f"$p is not adjecent to $pos")
    require(
      dirTo(p).opposite != direction,
      f"Cost to opposite direction is not defined"
    )
    if dirTo(p) == direction then COST_NEXT else COST_NEXT + COST_TURN

  def dirTo(p: Pos): Dir =
    require(pos.adjecent.contains(p), f"$p is not adjecent to $pos")
    if pos.north == p then Dir.North
    else if pos.east == p then Dir.East
    else if pos.south == p then Dir.South
    else Dir.West
end Node

def estimate(a: Pos, b: Pos): Int =
  val dx = Math.abs(a._1 - b._1)
  val dy = Math.abs(a._2 - b._2)
  (dx, dy) match
    case (0, y) => y * COST_NEXT
    case (x, 0) => x * COST_NEXT
    case _      => (dx + dy) * COST_NEXT + COST_TURN

def shortestPath(m: Maze)(start: Node, endPos: Pos): List[Node] =
  val open = PriorityQueue[Node](start)(
    Ordering.by[Node, Int](n => n.estimateTotal)((a, b) => b - a)
  )
  val closed = scala.collection.mutable.Set[Pos]()

  val path = boundary:
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
          .map((p, _) => (p, current.dirTo(p)))
          .filter((_, d) => d.opposite != current.direction)

        for (pos, dir) <- candidatePos do
          val costFromStart = current.costFromStart + current.costTo(pos)

          val betterExists =
            open.exists(n => n.pos == pos && n.costFromStart <= costFromStart)

          if !betterExists then
            open.enqueue(
              Node(
                pos,
                dir,
                costFromStart,
                estimateToEnd = estimate(pos, endPos),
                parent = Some(current),
                alternatives = candidatePos.map(_._1).filter(_ != pos)
              )
            )

  path match
    case x :: xs => x :: xs
    case _       => List()

def parse(input: Array[String]): Maze =
  Maze(input.map(_.toVector).toVector)

def alternatives(l: List[Node]): List[Pos] = l.flatMap(_.alternatives)

def solve1(input: Array[String]): Int =
  val m = parse(input).withoutDeadEnds
  val start = Node(
    m.startPos,
    Dir.East,
    0,
    estimate(m.startPos, m.endPos),
    None,
    Set()
  )
  shortestPath(m)(start, m.endPos).head.costFromStart

def solve1: Int = solve1(input)

def solve2(input: Array[String]): Int =
  val m = parse(input).withoutDeadEnds
  val start = Node(
    m.startPos,
    Dir.East,
    0,
    estimate(m.startPos, m.endPos),
    None,
    Set()
  )

  val first = shortestPath(m)(start, m.endPos)
  val bestPrice = first.head.costFromStart

  val paths = ListBuffer(first)
  var open = Set.from(alternatives(first))
  var closed = Set[Pos]()

  while open.nonEmpty do
    val currentPos = open.head
    closed += currentPos

    val part1 = shortestPath(m)(start, currentPos)
    val current =
      part1.head.copy(estimateToEnd = estimate(part1.head.pos, m.endPos))
    val part2 = shortestPath(m)(current, m.endPos)
    val newPath = part2 ++ part1

    newPath match
      case head :: next if head.costFromStart == bestPrice =>
        paths.addOne(head :: next)
        open = open ++ alternatives(head :: next)
      case _ => None

    open = open -- closed

  paths.flatten.map(_.pos).toSet.size

def solve2: Int = solve2(input)
