package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day9Suite extends AnyFunSuite {

  private val sample =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  private val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day9.task1(lines) == 114)
  }
}
