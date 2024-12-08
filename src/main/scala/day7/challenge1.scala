package me.cyrill.aoc2024.day7.challenge1

import me.cyrill.aoc2024.day7.*

val combinations: LazyList[List[List[Char]]] =
  List()
    #:: List(List('+'), List('*'))
    #:: combinations.tail
      .map(l => l.map(c => '+' :: c) ++ l.map(c => '*' :: c))

def solve(input: Array[String]): BigInt =
  sumSolutions(input)(combinations)
