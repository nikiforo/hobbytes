package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
final class Day12Suite extends AnyFunSuite {

  val sample =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1""".stripMargin

  val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day12.task1(lines) == 21)
  }

  test("task2") {
    assert(Day12.task2(lines) == 525152)
  }
}
