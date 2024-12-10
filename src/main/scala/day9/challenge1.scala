package me.cyrill.aoc2024.day9.challenge1

import me.cyrill.aoc2024.day9.*

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

def solve(input: String): Long =
  checksum:
    defragment:
      expand:
        input
