package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class Day1Suite extends AnyFunSuite {
  private val sample =
    """two1nine
       eightwothree
       abcone2threexyz
       xtwone3four
       4nineeightseven2
       zoneight234
       7pqrstsixteen"""

  private val sampleLines = sample.linesIterator.toList

  test("sample task") {
    assert(Day1.task2(sampleLines) == 281)
  }
}
