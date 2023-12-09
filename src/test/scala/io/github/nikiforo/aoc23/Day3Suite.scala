package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class Day3Suite extends AnyFunSuite {

  val sample =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin

  val lines = sample.linesIterator.toList

  test("expand") {
    assert(new Day3.Scheme(lines).fullNumberRange((0, 1)) == (0, 2))
  }

  test("string") {
    assert(new Day3.Scheme(lines).string(0, 0, 2) == "467")
  }

  test("task1") {
    assert(Day3.task1(lines) == 4361)
  }

  test("task2") {
    assert(Day3.task2(lines) == 467835)
  }
}
