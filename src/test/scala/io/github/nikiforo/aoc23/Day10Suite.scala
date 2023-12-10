package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day10Suite extends AnyFunSuite {

  val sample =
    """..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...""".stripMargin

  val sample2 =
    """...........
      |.S-------7.
      |.|F-----7|.
      |.||OOOOO||.
      |.||OOOOO||.
      |.|L-7OF-J|.
      |.|II|O|II|.
      |.L--JOL--J.
      |.....O.....""".stripMargin

  test("task1") {
    assert(Day10.task1(sample.linesIterator.toList) == 8)
  }

  test("task2 sample2") {
    assert(Day10.task2(sample2.linesIterator.toList) == 4)
  }
}
