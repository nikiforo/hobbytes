package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day16Suite extends AnyFunSuite {

  val sample =
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin

  val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day16.task1(lines) == 46)
  }
}
