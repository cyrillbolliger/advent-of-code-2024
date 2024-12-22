package me.cyrill.aoc2024.util.matrix

import me.cyrill.aoc2024.util.pos.*

import scala.reflect.ClassTag
import scala.collection.mutable.ArraySeq

trait BaseMatrix[T: ClassTag, S[T] <: scala.collection.IndexedSeq[T]](
    data: S[T],
    width: Int,
    height: Int
):
  lazy val positions = for
    y <- 0 until height
    x <- 0 until width
  yield (x, y)

  def apply(pos: Pos): T =
    throwIfOutOfBounds(pos)
    data(toIdx(pos))

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

  def hasPos(pos: Pos): Boolean =
    val (x, y) = pos
    x >= 0 && x < width && y >= 0 && y < height

  protected def toPos(idx: Int): Pos = (idx % width, idx / width)

  protected def toIdx(pos: Pos) =
    val (x, y) = pos: (Int, Int)
    x + width * y

  protected def throwIfOutOfBounds(pos: Pos): Unit =
    if !hasPos(pos) then
      throw IllegalArgumentException(
        f"pos$pos out of bounds: min(0, 0), max(${width - 1}, ${height - 1})"
      )

  override def toString(): String =
    data
      .grouped(width)
      .map(_.foldLeft("")(_.toString + _.toString))
      .reduce(_ + "\n" + _)

class MutableMatrix[T: ClassTag](
    protected val data: Array[T],
    val width: Int,
    val height: Int
) extends BaseMatrix[T, ArraySeq](data, width, height):
  def this(input: Array[Array[T]]) =
    this(input.flatten, input(0).size, input.size)

  def update(pos: Pos, value: T): Unit =
    throwIfOutOfBounds(pos)
    data(toIdx(pos)) = value

case class Matrix[T: ClassTag, S <: IndexedSeq](
    protected val data: IndexedSeq[T],
    val width: Int,
    val height: Int
) extends BaseMatrix[T, IndexedSeq](data, width, height):
  def this(input: S[S[T]]) =
    this(input.flatten, input(0).size, input.size)

  def updated(pos: Pos, value: T): Matrix[T, S] =
    throwIfOutOfBounds(pos)
    this.copy(data = data.updated(toIdx(pos), value))
