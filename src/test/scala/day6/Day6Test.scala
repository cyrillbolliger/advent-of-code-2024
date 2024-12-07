package me.cyrill.aoc2024.day6

import me.cyrill.aoc2024.day6.challenge2.step
import me.cyrill.aoc2024.day6.challenge2.Inspection

class Day6Test extends munit.FunSuite:
  test("OffMap: east"):
    val m = AMap(Array("..", "->", ".."))
    assertEquals(step(m), Inspection.OffMap)
    assertEquals(m.toString, AMap(Array("..", "->", "..")).toString)

  test("OffMap: south"):
    val m = AMap(Array("..", "|.", "v."))
    assertEquals(step(m), Inspection.OffMap)
    assertEquals(m.toString, AMap(Array("..", "|.", "v.")).toString)

  test("OffMap: west"):
    val m = AMap(Array("<-", "..", ".."))
    assertEquals(step(m), Inspection.OffMap)
    assertEquals(m.toString, AMap(Array("<-", "..", "..")).toString)

  test("OffMap: north"):
    val m = AMap(Array(".^", ".|", ".."))
    assertEquals(step(m), Inspection.OffMap)
    assertEquals(m.toString, AMap(Array(".^", ".|", "..")).toString)

  test("Ongoing: ."):
    val m = AMap(Array("...", ">..", "..."))
    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "->.", "...")).toString)

    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "-->", "...")).toString)

  test("Ongoing: -"):
    val m = AMap(Array("...", ">-.", "..."))
    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "->.", "...")).toString)

    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "-->", "...")).toString)

  test("Ongoing: |"):
    val m = AMap(Array("...", ">|.", "..."))
    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "->.", "...")).toString)

    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "-+>", "...")).toString)

  test("Ongoing: +"):
    val m = AMap(Array("...", ">+.", "..."))
    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "->.", "...")).toString)

    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "-+>", "...")).toString)

  test("Ongoing: #"):
    val m = AMap(Array("...", ">.#", "..."))
    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "->#", "...")).toString)

    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "-+#", ".v.")).toString)

  test("Ongoing: O"):
    val m = AMap(Array("...", ">.O", "..."))
    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "->O", "...")).toString)

    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "-+O", ".v.")).toString)

  test("Ongoing: Turn twice"):
    // format: off
    val m = AMap(Array(
        "...", 
        ">.#", 
        ".#."))
    // format: on
    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "->#", ".#.")).toString)

    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array("...", "<+#", ".#.")).toString)

  test("Ongoing: Turn three times"):
    // format: off
    val m = AMap(Array(
        "...", 
        "#>#", 
        ".#."))
    // format: on
    assertEquals(step(m), Inspection.Ongoing)
    assertEquals(m.toString, AMap(Array(".^.", "#+#", ".#.")).toString)

  test("Trapped: Turn four times"):
    // format: off
    val m = AMap(Array(
        ".#.", 
        "#>#", 
        ".#."))
    // format: on
    assertEquals(step(m), Inspection.Trapped)

  test("Trapped: Simple loop"):
    // format: off
    val m = AMap(
      Array(
        ">..#", 
        ".#..", 
        "...#", 
        "#...", 
        "..#."
      )
    )

    val steps = Array(
      AMap(Array(
        "->.#", 
        ".#..", 
        "...#", 
        "#...", 
        "..#.")),
      AMap(Array(
        "-->#", 
        ".#..", 
        "...#", 
        "#...", 
        "..#.")),
      AMap(Array(
        "--+#", 
        ".#v.", 
        "...#", 
        "#...", 
        "..#.")),
      AMap(Array(
        "--+#", 
        ".#|.", 
        "..v#", 
        "#...", 
        "..#.")),
      AMap(Array(
        "--+#", 
        ".#|.", 
        "..|#", 
        "#.v.", 
        "..#.")),
      AMap(Array(
        "--+#", 
        ".#|.", 
        "..|#", 
        "#<+.", 
        "..#.")),
      AMap(Array(
        "--+#", 
        ".#|.", 
        ".^|#", 
        "#++.", 
        "..#.")),
      AMap(Array(
        "--+#", 
        ".#|.", 
        ".+>#", 
        "#++.", 
        "..#."))
    )
    // format: on

    steps.foreach(s =>
      step(m)
      assertEquals(m.toString, s.toString)
    )

    assertEquals(step(m), Inspection.Trapped)
