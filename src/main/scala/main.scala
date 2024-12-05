package me.cyrill.aoc2024

@main
def main(): Unit = {
  println(f"Day 1, Challenge 1: ${day1.getTotalDistance()}")
  println(f"Day 1, Challenge 2: ${day1.getSimilarityScore()}")
  println(f"Day 2, Challenge 1: ${day2.getSafeCount()}")
  println(f"Day 2, Challenge 2: ${day2.getSafeCountWithTolerance()}")
  println(f"Day 3, Challenge 1: ${day3.solve1}")
  println(f"Day 3, Challenge 2: ${day3.solve2}")
  println(f"Day 4, Challenge 1: ${day4.solve1}")
  println(f"Day 4, Challenge 2: ${day4.solve2}")
  println(f"Day 1, Challenge 1: ${day5.solve1}")
  println(f"Day 1, Challenge 2: ${day5.solve2}")
}
