package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day20Suite extends AnyFunSuite {

  val sample =
    """broadcaster -> a, b, c
      |%a -> b
      |%b -> c
      |%c -> inv
      |&inv -> a""".stripMargin.linesIterator.toList

  val sample2 =
    """broadcaster -> a
      |%a -> inv, con
      |&inv -> b
      |%b -> con
      |&con -> output""".stripMargin.linesIterator.toList

  test("task1 sample") {
    assert(Day20.task1(sample) == 32000000)
  }

  test("task1 sample2") {
    assert(Day20.task1(sample2) == 11687500)
  }
}
