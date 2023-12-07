package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day7Suite extends AnyFunSuite {

  val sample =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day7.task1(lines) == 6440)
  }
}
