package me.cyrill.aoc2024.day7.challenge1

def parse(l: String): (List[BigInt], BigInt) =
  val s = l.split(":")
  val result = BigInt(s(0))
  val inputs = s(1).trim().split(" ").map(BigInt(_)).toList
  (inputs, result)

// todo: cache
def combinations(l: List[BigInt]): List[List[Char]] =
  val size = l.length - 1
  (for
    i <- 0 to size
    l = List.fill(i)('+') ++ List.fill(size - i)('*')
  yield l.permutations.toList).flatten.toList

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

def hasSolution(n: List[BigInt], s: BigInt): Boolean =
  combinations(n).exists(op => eval(n, op) == s)

def solve(input: Array[String]): BigInt =
  input
    .map(parse(_))
    .filter(hasSolution(_, _))
    .foldLeft(BigInt(0))(_ + _._2)
