package me.cyrill.aoc2024.day4

type Matrix = Array[Array[Char]]

extension (m: Matrix)
  def hMatch: Boolean =
    m(0)(0) == 'X'
      && m(0)(1) == 'M'
      && m(0)(2) == 'A'
      && m(0)(3) == 'S'

  def hRevMatch: Boolean =
    m(0)(0) == 'S'
      && m(0)(1) == 'A'
      && m(0)(2) == 'M'
      && m(0)(3) == 'X'

  def vMatch: Boolean =
    m(0)(0) == 'X'
      && m(1)(0) == 'M'
      && m(2)(0) == 'A'
      && m(3)(0) == 'S'

  def vRevMatch: Boolean =
    m(0)(0) == 'S'
      && m(1)(0) == 'A'
      && m(2)(0) == 'M'
      && m(3)(0) == 'X'

  def tlBrMatch: Boolean =
    m(0)(0) == 'X'
      && m(1)(1) == 'M'
      && m(2)(2) == 'A'
      && m(3)(3) == 'S'

  def tlBrRevMatch: Boolean =
    m(0)(0) == 'S'
      && m(1)(1) == 'A'
      && m(2)(2) == 'M'
      && m(3)(3) == 'X'

  def trBlMatch: Boolean =
    m(0)(3) == 'X'
      && m(1)(2) == 'M'
      && m(2)(1) == 'A'
      && m(3)(0) == 'S'

  def trBlRevMatch: Boolean =
    m(0)(3) == 'S'
      && m(1)(2) == 'A'
      && m(2)(1) == 'M'
      && m(3)(0) == 'X'
