package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

final class Day14Suite extends AnyFunSuite {
  val sample =
    """O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....""".stripMargin
  val lines = sample.linesIterator.toList

  test("task1") {
    assert(Day14.task1(lines) == 136)
  }
}
