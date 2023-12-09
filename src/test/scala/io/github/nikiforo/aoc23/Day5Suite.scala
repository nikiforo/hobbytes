package io.github.nikiforo.aoc23

import cats.syntax.either._
import org.scalatest.funsuite.AnyFunSuite
import io.github.nikiforo.aoc23.Day5.AlmRange
import io.github.nikiforo.aoc23.Day5.SeedRange
import org.scalatest.DoNotDiscover

@DoNotDiscover
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

  test("applyRange") {
    assert(Day5.intersectRanges(SeedRange(98, 98), AlmRange(50, 98, 2)) == Some((SeedRange(50, 50), List())))
  }

  test("applyRange2") {
    assert(Day5.intersectRanges(SeedRange(53, 53), AlmRange(52, 50, 48)) == Some((SeedRange(55, 55), List())))
  }

  test("applyRange3") {
    val results = (SeedRange(50, 51), List(SeedRange(95, 97), SeedRange(100, 100)))
    assert(Day5.intersectRanges(SeedRange(95, 100), AlmRange(50, 98, 2)) == Some(results))
  }

  test("task2") {
    assert(Day5.task2(lines) == 46)
  }
}
