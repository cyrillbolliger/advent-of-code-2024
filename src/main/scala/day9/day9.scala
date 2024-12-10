package me.cyrill.aoc2024.day9

import scala.io.Source

val inputPath = "src/main/scala/day9/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray.apply(0)

enum Block:
  case Used(len: Int, id: Int)
  case Free(len: Int)

  def isFree: Boolean = this match
    case Free(_) => true
    case _       => false
  def isUsed: Boolean = !this.isFree
  def getId: Int = this match
    case Used(_, id) => id
    case Free(_)     => throw Exception("Free blocks have no id")
  def getLen: Int = this match
    case Used(len, id) => len
    case Free(len)     => len

def expand(input: String): Seq[Block] =
  (for
    idx <- 0 until input.length
    len = input(idx).toString.toInt
    block =
      if idx % 2 == 0 then Block.Used(len, idx / 2)
      else Block.Free(len)
  yield Seq.fill(len)(block)).flatten

def checksum(s: Seq[Block]): Long =
  s.zipWithIndex.foldLeft(0L)((acc, item) =>
    val (block, idx) = item
    val p = block match
      case Block.Used(len, id) => id * idx
      case Block.Free(len)     => 0

    acc + p
  )

def solve1 = challenge1.solve(input)
def solve2 = challenge2.solve(input)
