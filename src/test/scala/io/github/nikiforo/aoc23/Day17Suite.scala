package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class Day17Suite extends AnyFunSuite {
  val sample =
    """2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin

  val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day17.task1(lines) == 102)
  }

  test("task2") {
    assert(Day17.task2(lines) == 94)
  }
}
