package io.github.nikiforo.aoc23

import cats.syntax.either._
import org.scalatest.funsuite.AnyFunSuite

class Day5Suite extends AnyFunSuite {
  val sample =
    """seeds: 79 14 55 13

      |seed-to-soil map:
      |50 98 2
      |52 50 48

      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15

      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4

      |water-to-light map:
      |88 18 7
      |18 25 70

      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13

      |temperature-to-humidity map:
      |0 69 1
      |1 0 69

      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin

  val lines = sample.linesIterator.toList

  test("parses") {
    assert(Either.catchNonFatal(Day5.parse(lines)).isRight)
  }

  test("applyRange") {
    assert(Day5.applyRange(98, Day5.AlmRange(50, 98, 2)) == Some(50))
  }

  test("applyRange2") {
    assert(Day5.applyRange(53, Day5.AlmRange(52, 50, 48)) == Some(55))
  }
}
