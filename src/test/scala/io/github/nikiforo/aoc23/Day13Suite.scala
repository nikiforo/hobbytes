package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

final class Day13Suite extends AnyFunSuite {

  val sample =
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.

      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#""".stripMargin

  val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day13.task1(lines) == 405)
  }

  test("task2") {
    assert(Day13.task2(lines) == 400)
  }
}
