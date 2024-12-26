package me.cyrill.aoc2024.day17

import scala.io.Source
import scala.util.boundary, boundary.break

case class State(ra: Long, rb: Long, rc: Long, ip: Int, val result: List[Int]):
  def exec(op: Int, arg: Int): State =
    op match
      case 0 => adv(arg)
      case 1 => bxl(arg)
      case 2 => bst(arg)
      case 3 => jnz(arg)
      case 4 => bxc(arg)
      case 5 => out(arg)
      case 6 => bdv(arg)
      case 7 => cdv(arg)
      case _ => throw IllegalArgumentException(f"Undefined opcode: $op")

  // combo operands:
  //   0 - 3: literal value
  //   4: ra
  //   5: rb
  //   6: rc
  //   7: reserved
  def combo(arg: Int): Long =
    arg match
      case 4                     => ra
      case 5                     => rb
      case 6                     => rc
      case d if d >= 0 && d <= 3 => d
      case _ => throw IllegalArgumentException(f"Undefined combo operand: $arg")

  def nextInstr: Int = ip + 2

  def divPower2(n: Long, d: Long): Long =
    n / Math.pow(2, d.toDouble).toLong

  def mod8(arg: Long): Int =
    (arg % 8).toInt

  // 0: adv -> division -> ra / (2^combo_op) -> truncated to int -> ra
  def adv(arg: Int): State =
    this.copy(
      ra = divPower2(ra, combo(arg)),
      ip = nextInstr
    )

  // 1: bxl -> bitwise xor -> rb xor literal_op -> rb
  def bxl(arg: Int): State =
    this.copy(
      rb = rb ^ arg,
      ip = nextInstr
    )

  // 2: bst -> combo_op % 8 -> lowest 3 bits -> rb
  def bst(arg: Int): State =
    this.copy(
      rb = mod8(combo(arg)) & 7, // 0b111 == 7
      ip = nextInstr
    )

  // 3: jnz -> if ra != 0 then ip = literal_op (do not increase ip after)
  def jnz(arg: Int): State =
    if ra == 0 then this.copy(ip = nextInstr)
    else this.copy(ip = arg)

  // 4: bxc -> bitwise xor -> rb xor rc -> rb (ignore operand)
  def bxc(arg: Int): State =
    this.copy(
      rb = rb ^ rc,
      ip = nextInstr
    )

  // 5: out -> combo_op % 8 -> print
  def out(arg: Int): State =
    this.copy(
      result = mod8(combo(arg)) :: result,
      ip = nextInstr
    )

  // 6: bdv -> division -> ra / (2^combo_op) -> truncated (!) to int -> rb
  def bdv(arg: Int): State =
    this.copy(
      rb = divPower2(ra, combo(arg)),
      ip = nextInstr
    )

  // 7: cdv -> division -> ra / (2^combo_op) -> truncated (!) to int -> rc
  def cdv(arg: Int): State =
    this.copy(
      rc = divPower2(ra, combo(arg)),
      ip = nextInstr
    )

val initialState = State(30878003, 0, 0, 0, List())

// (opcode, operand); universe: 0..7
val program = Vector(2, 4, 1, 2, 7, 5, 0, 3, 4, 7, 1, 7, 5, 5, 3, 0)

def solve(state: State): List[Int] =
  var s = state
  while s.ip < program.size do
    val op = program(s.ip)
    val arg = program(s.ip + 1)
    s = s.exec(op, arg)
  s.result.reverse

def solve1: String =
  solve(initialState).map(_.toString).reduceLeft(_ + "," + _)

def increment(exp: Int): Long = Math.pow(8, exp).toLong

def solve2: Long =
  var exp = 15
  var i = increment(exp)

  boundary:
    while i <= Long.MaxValue do
      val res = solve(State(i, 0, 0, 0, List()))
      if res == program then break(i)
      if res(exp) == program(exp) then exp -= 1
      else i += increment(exp)
    -1L
