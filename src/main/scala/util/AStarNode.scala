package me.cyrill.aoc2024.util.astarnode

import scala.collection.mutable.PriorityQueue
import scala.util.boundary, boundary.break

final case class AStarNode[T](
    val pos: T,
    val costFromStart: Int,
    val estimateToEnd: Int,
    val parent: Option[AStarNode[T]]
) extends Ordered[AStarNode[T]]:
  val estimateTotal = costFromStart + estimateToEnd

  lazy val toStart: List[AStarNode[T]] = parent match
    case None    => this :: Nil
    case Some(p) => this :: p.toStart

  override def compare(that: AStarNode[T]): Int =
    that.estimateTotal.compare(this.estimateTotal)
end AStarNode

def shortestPath[T](estimate: (T, T) => Int)(neighbours: T => Iterable[T])(
    a: T,
    b: T
): List[AStarNode[T]] =
  val start = AStarNode(a, 0, estimate(a, b), None)
  val open = PriorityQueue(start)
  val closed = scala.collection.mutable.Set[T]()

  boundary:
    while open.nonEmpty do
      val current = open.dequeue()

      if current.pos == b then break(current.toStart)
      else
        closed.addOne(current.pos)

        val candidates = neighbours(current.pos).filter(!closed.contains(_))

        for pos <- candidates do
          val costFromStart = current.costFromStart + 1
          val betterExists =
            open.exists(n => n.pos == pos && n.costFromStart <= costFromStart)
          if (!betterExists) then
            open.enqueue(
              AStarNode(
                pos,
                costFromStart,
                estimate(pos, b),
                Some(current)
              )
            )
    List()
