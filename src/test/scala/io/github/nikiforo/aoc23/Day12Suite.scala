package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

final class Day12Suite extends AnyFunSuite {

  val sample =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1""".stripMargin

  val lines = sample.linesIterator.toList

  test("task1 line1") {
    val (chars, next) = Day12.parse(lines(0))
    assert(Day12.count(chars, -1, next) == 1)
  }

  test("task1 line2") {
    val (chars, next) = Day12.parse(lines(1))
    assert(Day12.count(chars, -1, next) == 4)
  }

  test("task1") {
    assert(Day12.task1(lines) == 21)
  }
}
