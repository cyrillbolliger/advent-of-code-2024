package me.cyrill.aoc2024.day7.challenge1

def parse(l: String): (List[BigInt], BigInt) =
  val s = l.split(":")
  val result = BigInt(s(0))
  val inputs = s(1).trim().split(" ").map(BigInt(_)).toList
  (inputs, result)

val combinations: LazyList[List[List[Char]]] =
  List()
    #:: List(List('+'), List('*'))
    #:: combinations.tail
      .map(l => l.map(c => '+' :: c) ++ l.map(c => '*' :: c))

def eval(n: List[BigInt], op: List[Char]): BigInt =
  require(op.length == n.length - 1)
  require(op.forall(o => o == '+' || o == '*'))

  n.drop(1)
    .zipWithIndex
    .foldLeft(n(0))((acc, item) =>
      val (num, idx) = item
      op(idx) match
        case '+' => acc + num
        case '*' => acc * num
    )

def hasSolution(l: List[BigInt], s: BigInt): Boolean =
  combinations(l.length - 1).exists(op => eval(l, op) == s)

def solve(input: Array[String]): BigInt =
  input
    .map(parse(_))
    .filter(hasSolution(_, _))
    .foldLeft(BigInt(0))(_ + _._2)
