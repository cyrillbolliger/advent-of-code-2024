package me.cyrill.aoc2024.day9.challenge1

import me.cyrill.aoc2024.day9.*

enum Block:
  case Used(id: Int)
  case Free

  def isFree: Boolean = this == Free
  def isUsed: Boolean = !this.isFree
  def getId: Int = this match
    case Used(id) => id
    case Free     => throw Exception("Free blocks have no id")

def expand(input: String): Seq[Block] =
  (for
    idx <- 0 until input.length
    len = input(idx).toString.toInt
    block =
      if idx % 2 == 0 then Block.Used(idx / 2)
      else Block.Free
  yield Seq.fill(len)(block)).flatten

def defragment(s: Seq[Block]): Seq[Block] =
  var usedCount = s.count(_.isUsed)
  var j = s.size
  for
    i <- 0 until usedCount
    block = s(i)
    data =
      if block.isUsed then block
      else
        j = s.take(j).lastIndexWhere(_.isUsed)
        s(j)
  yield data

def checksum(s: Seq[Block]): Long =
  s.zipWithIndex.foldLeft(0L)((acc, item) =>
    val (block, idx) = item
    acc + block.getId * idx
  )

def solve(input: String): Long =
  checksum:
    defragment:
      expand:
        input
