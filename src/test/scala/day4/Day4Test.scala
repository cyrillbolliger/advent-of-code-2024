package me.cyrill.aoc2024.day4

class Day4Test extends munit.FunSuite:
  def Matrix = Array.fill(4)(Array.fill(4)('.'))

  val horizontal = Matrix
  horizontal(0)(0) = 'X'
  horizontal(0)(1) = 'M'
  horizontal(0)(2) = 'A'
  horizontal(0)(3) = 'S'

  val horizontalReversed = Matrix
  horizontalReversed(0)(0) = 'S'
  horizontalReversed(0)(1) = 'A'
  horizontalReversed(0)(2) = 'M'
  horizontalReversed(0)(3) = 'X'

  val vertical = Matrix
  vertical(0)(0) = 'X'
  vertical(1)(0) = 'M'
  vertical(2)(0) = 'A'
  vertical(3)(0) = 'S'

  val verticalReversed = Matrix
  verticalReversed(0)(0) = 'S'
  verticalReversed(1)(0) = 'A'
  verticalReversed(2)(0) = 'M'
  verticalReversed(3)(0) = 'X'

  val diagonal1 = Matrix
  diagonal1(0)(0) = 'X'
  diagonal1(1)(1) = 'M'
  diagonal1(2)(2) = 'A'
  diagonal1(3)(3) = 'S'

  val diagonal1Reversed = Matrix
  diagonal1Reversed(0)(0) = 'S'
  diagonal1Reversed(1)(1) = 'A'
  diagonal1Reversed(2)(2) = 'M'
  diagonal1Reversed(3)(3) = 'X'

  val diagonal2 = Matrix
  diagonal2(0)(3) = 'X'
  diagonal2(1)(2) = 'M'
  diagonal2(2)(1) = 'A'
  diagonal2(3)(0) = 'S'

  val diagonal2Reversed = Matrix
  diagonal2Reversed(0)(3) = 'S'
  diagonal2Reversed(1)(2) = 'A'
  diagonal2Reversed(2)(1) = 'M'
  diagonal2Reversed(3)(0) = 'X'

  test("hMatch"):
    assert(horizontal.hMatch)
    assert(!horizontalReversed.hMatch)

  test("hRevMatch"):
    assert(horizontalReversed.hRevMatch)
    assert(!horizontal.hRevMatch)

  test("vMatch"):
    assert(vertical.vMatch)
    assert(!verticalReversed.vMatch)

  test("vRevMatch"):
    assert(verticalReversed.vRevMatch)
    assert(!vertical.vRevMatch)

  test("tlBrMatch"):
    assert(diagonal1.tlBrMatch)
    assert(!diagonal1Reversed.tlBrMatch)

  test("tlBrRevMatch"):
    assert(diagonal1Reversed.tlBrRevMatch)
    assert(!diagonal1.tlBrRevMatch)

  test("trBlMatch"):
    assert(diagonal2.trBlMatch)
    assert(!diagonal2Reversed.trBlMatch)

  test("trBlRevMatch"):
    assert(diagonal2Reversed.trBlRevMatch)
    assert(!diagonal2.trBlRevMatch)

  test("RowBuffer"):
    val rb = RowBuffer("0", "1", "2", "3")
    for i <- 0 until 5 do
      for j <- 0 until 4 do assertEquals(rb(j), (j + i).toString)
      rb.push((4 + i).toString)

  test("countInMatrix: 1"):
    assertEquals(countInMatrix(horizontal), 1)
    assertEquals(countInMatrix(horizontalReversed), 1)
    assertEquals(countInMatrix(vertical), 1)
    assertEquals(countInMatrix(verticalReversed), 1)
    assertEquals(countInMatrix(diagonal1), 1)
    assertEquals(countInMatrix(diagonal1Reversed), 1)

  test("countInMatrix: 3"):
    val m = Matrix
    m(0)(0) = 'X'
    m(1)(1) = 'M'
    m(1)(0) = 'M'
    m(0)(1) = 'M'
    m(2)(2) = 'A'
    m(2)(0) = 'A'
    m(0)(2) = 'A'
    m(3)(3) = 'S'
    m(3)(0) = 'S'
    m(0)(3) = 'S'
    assertEquals(countInMatrix(m), 3)

  test("countInMatrix reversed: 3"):
    val m = Matrix
    m(0)(0) = 'S'
    m(0)(3) = 'S'
    m(1)(0) = 'A'
    m(1)(2) = 'A'
    m(2)(0) = 'M'
    m(2)(1) = 'M'
    m(3)(0) = 'X'
    m(3)(1) = 'M'
    m(3)(2) = 'A'
    m(3)(3) = 'S'
    assertEquals(countInMatrix(m), 2)

  test("countInRows: 5"):
    val rows = Array(
      "X.S..S.X",
      "M.A.A..M",
      "A.MM...A",
      "S.XMAS.S" // this xmas must not be counted
    )
    assertEquals(
      countInRows(rows(0), rows(1), rows(2), rows(3)),
      4
    )

  test("countInStringIterator: test data"):
    val testData = Array(
      "MMMSXXMASM",
      "MSAMXMSMSA",
      "AMXSXMAAMM",
      "MSAMASMSMX",
      "XMASAMXAMM",
      "XXAMMXXAMA",
      "SMSMSASXSS",
      "SAXAMASAAA",
      "MAMMMXMMMM",
      "MXMXAXMASX"
    )

    assertEquals(
      countInStringIterator(testData.iterator),
      18
    )
