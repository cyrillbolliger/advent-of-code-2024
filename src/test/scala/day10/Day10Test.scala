package me.cyrill.aoc2024.day10

import me.cyrill.aoc2024.day10.*
import challenge1.*

class Day10Test extends munit.FunSuite:
  val testInput = Array(
    "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732"
  )

  test("challenge1.solve"):
    assertEquals(challenge1.solve(testInput), 36)

  test("parse"):
    assertEquals(parse(Array("12", "34")), Vector(Vector(1, 2), Vector(3, 4)))

  test("TMap.get: onMap"):
    val m = parse(Array("12", "34"))
    assertEquals(m.get(Coord(0, 0)).get, Cell(1, Coord(0, 0))(m))
    assertEquals(m.get(Coord(1, 0)).get, Cell(2, Coord(1, 0))(m))
    assertEquals(m.get(Coord(0, 1)).get, Cell(3, Coord(0, 1))(m))
    assertEquals(m.get(Coord(1, 1)).get, Cell(4, Coord(1, 1))(m))

  test("TMap.get: offMap"):
    val m = parse(Array("12", "34"))
    assert(m.get(Coord(-1, 0)).isEmpty)
    assert(m.get(Coord(2, 0)).isEmpty)
    assert(m.get(Coord(0, -1)).isEmpty)
    assert(m.get(Coord(0, 2)).isEmpty)

  test("Coord.neighbours"):
    assertEquals(
      Coord(0, 0).neighbours,
      Neighbours(
        Coord(0, -1),
        Coord(1, 0),
        Coord(0, 1),
        Coord(-1, 0)
      )
    )

  test("Cell.neighbours"):
    val m = parse(Array("12", "34"))
    assertEquals(
      m.get(Coord(0, 1)).get.neighbours,
      Neighbours(
        Some(Cell(1, Coord(0, 0))(m)),
        Some(Cell(4, Coord(1, 1))(m)),
        None,
        None
      )
    )

  test("Node.neighbours: none"):
    val m = parse(Array("111", "101", "111"))
    assertEquals(
      Node(m.get(Coord(1, 1)).get).neighbours,
      Neighbours[Option[Node]](
        None,
        None,
        None,
        None
      )
    )

  test("Node.neighbours: all"):
    val m = parse(Array("111", "121", "111"))
    assertEquals(
      Node(m.get(Coord(1, 1)).get).neighbours,
      Neighbours[Option[Node]](
        Some(Node(m.get(Coord(1, 0)).get)),
        Some(Node(m.get(Coord(2, 1)).get)),
        Some(Node(m.get(Coord(1, 2)).get)),
        Some(Node(m.get(Coord(0, 1)).get))
      )
    )

  test("Node.neighbours: north"):
    val m = parse(Array("121", "131", "111"))
    assertEquals(
      Node(m.get(Coord(1, 1)).get).neighbours,
      Neighbours[Option[Node]](
        Some(Node(m.get(Coord(1, 0)).get)),
        None,
        None,
        None
      )
    )

  test("Node.neighbours: west"):
    val m = parse(Array("111", "132", "111"))
    assertEquals(
      Node(m.get(Coord(1, 1)).get).neighbours,
      Neighbours[Option[Node]](
        None,
        Some(Node(m.get(Coord(2, 1)).get)),
        None,
        None
      )
    )

  test("Node.neighbours: south"):
    val m = parse(Array("111", "131", "121"))
    assertEquals(
      Node(m.get(Coord(1, 1)).get).neighbours,
      Neighbours[Option[Node]](
        None,
        None,
        Some(Node(m.get(Coord(1, 2)).get)),
        None
      )
    )

  test("Node.neighbours: east"):
    val m = parse(Array("111", "231", "111"))
    assertEquals(
      Node(m.get(Coord(1, 1)).get).neighbours,
      Neighbours[Option[Node]](
        None,
        None,
        None,
        Some(Node(m.get(Coord(0, 1)).get))
      )
    )

  test("Node.paths: 0"):
    val m = parse(Array("111", "101", "111"))
    assertEquals(
      Node(m.get(Coord(1, 1)).get).paths,
      Set()
    )

  test("Node.paths: dist 1"):
    val m = parse(Array("000", "010", "000"))
    assertEquals(
      Node(m.get(Coord(1, 1)).get).paths,
      Set(
        Path(Coord(1, 0), Coord(1, 1)),
        Path(Coord(2, 1), Coord(1, 1)),
        Path(Coord(1, 2), Coord(1, 1)),
        Path(Coord(0, 1), Coord(1, 1))
      )
    )

  test("Node.paths: dist 2 1-D"):
    val m = parse(Array("012"))
    assertEquals(
      Node(m.get(Coord(2, 0)).get).paths,
      Set(Path(Coord(0, 0), Coord(2, 0)))
    )

  test("Node.paths: dist 2 2-D"):
    val m = parse(
      Array(
        "010",
        "121"
      )
    )
    assertEquals(
      Node(m.get(Coord(1, 1)).get).paths,
      Set(Path(Coord(0, 0), Coord(1, 1)), Path(Coord(2, 0), Coord(1, 1)))
    )
