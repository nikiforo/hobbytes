package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

final class Day21Suite extends AnyFunSuite {

  val sample =
    """...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin.linesIterator.toList

  test("run1 sample") {
    assert(Day21.run1(sample, 6) == 16)
  }
}
