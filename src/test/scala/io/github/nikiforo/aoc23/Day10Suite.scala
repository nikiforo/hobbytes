package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day10Suite extends AnyFunSuite {

  val sample =
    """..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...""".stripMargin

  val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day10.task1(lines) == 8)
  }
}
