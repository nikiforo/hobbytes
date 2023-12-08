package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day8Suite extends AnyFunSuite {
  val sample =
    """RL

      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val lines = sample.linesIterator.toList

  val sample2 =
    """LLR

      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val lines2 = sample2.linesIterator.toList

  val sample3 =
    """LR

      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)""".stripMargin

  val lines3 = sample3.linesIterator.toList

  test("task1 2") {
    assert(Day8.task1(lines) == 2)
  }

  test("task1 ") {
    assert(Day8.task1(lines2) == 6)
  }

  test("task 2") {
    assert(Day8.task2(lines3) == 6)
  }
}
