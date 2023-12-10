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

  val sample3 =
    """.F----7F7F7F7F-7....
      |.|F--7||||||||FJ....
      |.||.FJ||||||||L7....
      |FJL7L7LJLJ||LJ.L-7..
      |L--J.L7...LJS7F-7L7.
      |....F-J..F7FJ|L7L7L7
      |....L7.F7||L7|.L7L7|
      |.....|FJLJ|FJ|F7|.LJ
      |....FJL-7.||.||||...
      |....L---J.LJ.LJLJ...""".stripMargin

  test("task1") {
    assert(Day10.task1(sample.linesIterator.toList) == 8)
  }

  test("task2 sample2") {
    assert(Day10.task2(sample2.linesIterator.toList) == 4)
  }

  test("task2 sample3") {
    assert(Day10.task2(sample3.linesIterator.toList) == 8)
  }
}
