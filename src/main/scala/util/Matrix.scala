package me.cyrill.aoc2024.util.matrix

import me.cyrill.aoc2024.util.pos.*

import scala.reflect.ClassTag

class Matrix[T: ClassTag](private val input: Array[Array[T]]):
  require(input.size > 0)

  val height = input.size
  val width = input(0).size
  private val data: Array[T] = input.flatten

  lazy val positions = for
    y <- 0 until height
    x <- 0 until width
  yield (x, y)

  def apply(pos: Pos): T =
    throwIfOutOfBounds(pos)
    data(toIdx(pos))

  def update(pos: Pos, value: T): Unit =
    throwIfOutOfBounds(pos)
    data(toIdx(pos)) = value

  def hasPos(pos: Pos): Boolean =
    val (x, y) = pos
    x >= 0 && x < width && y >= 0 && y < height

  def adjecent(pos: Pos): Set[T] =
    throwIfOutOfBounds(pos)
    pos.adjecent
      .filter(hasPos)
      .map(apply)

  def surrounding(pos: Pos): Set[T] =
    throwIfOutOfBounds(pos)
    pos.surrounding
      .filter(hasPos)
      .map(apply)

  private def toPos(idx: Int): Pos = (idx % width, idx / width)

  private def toIdx(pos: Pos) =
    val (x, y) = pos: (Int, Int)
    x + width * y

  private def throwIfOutOfBounds(pos: Pos): Unit =
    if !hasPos(pos) then
      throw IllegalArgumentException(
        f"pos$pos out of bounds: min(0, 0), max(${width - 1}, ${height - 1})"
      )

  override def toString(): String =
    data
      .grouped(width)
      .map(_.foldLeft("")(_.toString + _.toString))
      .reduce(_ + "\n" + _)
