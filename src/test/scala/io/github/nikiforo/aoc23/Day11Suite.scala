package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class Day11Suite extends AnyFunSuite {

  private val sample =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin

  private val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day11.task1(lines) == 374)
  }

  test("task2") {
    assert(Day11.solve(lines, 100) == 8410)
  } 
}
