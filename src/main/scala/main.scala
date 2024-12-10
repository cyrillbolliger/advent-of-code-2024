package me.cyrill.aoc2024

import scala.collection.parallel.CollectionConverters.*

@main
def main(): Unit =
  val days = List(
    List(() => day1.getTotalDistance(), () => day1.getSimilarityScore()),
    List(() => day2.getSafeCount(), () => day2.getSafeCountWithTolerance()),
    List(() => day3.solve1, () => day3.solve2),
    List(() => day4.solve1, () => day4.solve2),
    List(() => day5.solve1, () => day5.solve2),
    List(() => day6.solve1, () => day6.solve2),
    List(() => day7.solve1, () => day7.solve2),
    List(() => day8.solve1, () => day8.solve2),
    List(() => day9.solve1, () => day9.solve2)
  )

  days.par // fork
    .map(l => l.par.map(eval)) // compute results
    .zipWithIndex
    .flatMap(d =>
      val (challenges, day) = d
      challenges.zipWithIndex.map(c =>
        val (e, challenge) = c
        val (result, duration) = e
        f"Day ${day + 1}, Challenge ${challenge + 1} (${duration}s) --> $result"
      )
    )
    .toList // join
    .sorted
    .map(println)

def eval(f: () => Any): (String, Double) =
  val start = System.currentTimeMillis()

  val res = f().toString

  val end = System.currentTimeMillis()
  val durationSeconds = ((end - start) / 100) / 10.0

  (res, durationSeconds)
