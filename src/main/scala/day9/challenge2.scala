package me.cyrill.aoc2024.day9.challenge2

import me.cyrill.aoc2024.day9.*

def defragment(s: Seq[Block]): Seq[Block] =
  val a = s.toArray
  var firstFree = 0
  var offset = a.size - 1
  while offset > 0 do
    firstFree = a.indexWhere(_.isFree, firstFree)
    val block = a(offset)
    if block.isUsed then
      val freeIdx =
        a.take(offset)
          .indexWhere(b => b.isFree && b.getLen >= block.getLen, firstFree)
      if freeIdx >= 0 then
        val free = a(freeIdx)
        val remainingFree = Block.Free(free.getLen - block.getLen)
        for i <- 0 until free.getLen do
          if a(offset - i).isUsed && a(offset - i).getId == block.getId then
            // copy block to free
            a(freeIdx + i) = a(offset - i)
          else
            // set the remaining free with the correct len
            a(freeIdx + i) = remainingFree
        for j <- 0 until block.getLen
        do
          // replace old block position with frees
          a(offset - j) = Block.Free(block.getLen)
    offset -= block.getLen
  a.toSeq

def solve(input: String): Long =
  checksum:
    defragment:
      expand:
        input
