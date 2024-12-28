package me.cyrill.aoc2024.day19

import scala.io.Source

val inputPath = "src/main/scala/day19/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

val rawPatterns = input(0)
val designs = input.drop(2).toList

given allPatterns: Map[Char, Vector[String]] = rawPatterns
  .split(',')
  .map(_.trim)
  .groupBy(_(0))
  .view
  .mapValues(_.sortBy(-_.length).toVector)
  .toMap
  .withDefault(_ => Vector.empty)

type NodeCache = scala.collection.mutable.Map[(String, String), Node]
given cache: NodeCache =
  scala.collection.mutable.Map[(String, String), Node]()

case class Node(str: String, pattern: String)(using
    allPatterns: Map[Char, Vector[String]]
)(using cache: NodeCache):
  val matches: Long = getMatches

  cache.addOne((str, pattern) -> this)

  def children: List[Node] =
    if str.isEmpty then Nil
    else childrenMatching(allPatterns(str(0))).filter(_.matches > 0)

  def childrenMatching(patterns: Vector[String]): List[Node] =
    patterns.zipWithIndex.find((p, _) => str.startsWith(p)) match
      case None => Nil
      case Some((p, idx)) =>
        val s = str.drop(p.size)
        val remainingPatterns = patterns.drop(idx + 1)
        val child = cache.getOrElse((s, p), Node(s, p))
        child :: childrenMatching(remainingPatterns)

  def getMatches: Long =
    if str.isEmpty then 1
    else children.map(_.matches).sum
end Node

lazy val solved = designs.map(d => Node(d, ""))

def solve1: Int =
  solved
    .filter(_.matches > 0)
    .size

def solve2: Long =
  solved
    .map(_.matches)
    .sum
