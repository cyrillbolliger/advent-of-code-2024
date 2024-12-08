package me.cyrill.aoc2024.day7

import scala.io.Source

val inputPath = "src/main/scala/day7/input.txt"
val input = Source.fromFile(inputPath).getLines().toArray

def parse(l: String): (List[BigInt], BigInt) =
  val s = l.split(":")
  val result = BigInt(s(0))
  val inputs = s(1).trim().split(" ").map(BigInt(_)).toList
  (inputs, result)

def eval(n: List[BigInt], op: List[Char]): BigInt =
  require(op.length == n.length - 1)
  require(op.forall(o => o == '+' || o == '*' || o == '|'))

  n.drop(1)
    .zipWithIndex
    .foldLeft(n(0))((acc, item) =>
      val (num, idx) = item
      op(idx) match
        case '+' => acc + num
        case '*' => acc * num
        case '|' => BigInt(acc.toString + num.toString)
    )

def hasSolution(
    combos: LazyList[List[List[Char]]]
)(l: List[BigInt], s: BigInt): Boolean =
  combos(l.length - 1).exists(op => eval(l, op) == s)

def sumSolutions(input: Array[String])(
    combos: LazyList[List[List[Char]]]
): BigInt =
  input
    .map(parse(_))
    .filter(hasSolution(combos)(_, _))
    .foldLeft(BigInt(0))(_ + _._2)

def solve1 = challenge1.solve(input)
def solve2 = challenge2.solve(input)
