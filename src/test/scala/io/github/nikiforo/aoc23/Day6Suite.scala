package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class Day6Suite extends AnyFunSuite {

  val sample =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day6.task1(lines) == 288)
  }

  test("task2") {
    assert(Day6.task2(lines) == 71503)
  }
}
