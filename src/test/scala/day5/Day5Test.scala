package me.cyrill.aoc2024.day5

class Day5Test extends munit.FunSuite:
  val testInput = Array(
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"
  )

  val rulesInput = Array(
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13"
  )

  val rules = Map(
    47 -> Set(53, 13, 61, 29),
    97 -> Set(13, 61, 47, 29, 53, 75),
    75 -> Set(29, 53, 47, 61, 13),
    61 -> Set(13, 53, 29),
    29 -> Set(13),
    53 -> Set(29, 13)
  )

  val jobsInput = Array(
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"
  )

  val jobs = Seq(
    List(75, 47, 61, 53, 29),
    List(97, 61, 53, 29, 13),
    List(75, 29, 13),
    List(75, 97, 47, 61, 53),
    List(61, 13, 29),
    List(97, 13, 75, 29, 47)
  )

  test("getInput"):
    val (r, j) = getInput(testInput)
    assertEquals(r.toSeq, rulesInput.toSeq)
    assertEquals(j.toSeq, jobsInput.toSeq)

  test("parseRules"):
    assertEquals(parseRules(rulesInput), rules)

  test("parseJobs"):
    assertEquals(parseJobs(jobsInput), jobs)

  test("checkJob"):
    assert(checkJob(jobs(0), rules))
    assert(checkJob(jobs(1), rules))
    assert(checkJob(jobs(2), rules))
    assert(!checkJob(jobs(3), rules))
    assert(!checkJob(jobs(4), rules))
    assert(!checkJob(jobs(5), rules))

  test("getMiddlePageNum"):
    assertEquals(getMiddlePageNum(List(1)), 1)
    assertEquals(getMiddlePageNum(List(1, 2, 3)), 2)
    assertEquals(getMiddlePageNum(List(1, 2, 3, 4, 5)), 3)
