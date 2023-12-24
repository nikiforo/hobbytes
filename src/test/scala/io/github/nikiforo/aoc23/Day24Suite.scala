package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day24Suite extends AnyFunSuite {
  import Day24.parse
  import Day24.Hail

  val sample =
    """19, 13, 30 @ -2, 1, -2
      |18, 19, 22 @ -1, -1, -2
      |20, 25, 34 @ -2, -2, -4
      |12, 31, 28 @ -1, -2, -1
      |20, 19, 15 @ 1, -5, -3""".stripMargin.linesIterator.toList

  test("intersections") {
    val hail1 = (Hail(19,13,30,-2,1,-2), Hail(18,19,22,-1,-1,-2))
    val hail2 = (Hail(19,13,30,-2,1,-2), Hail(20,25,34,-2,-2,-4))
    assert(Day24.intersections(sample.map(parse).toArray, 7, 27) == Vector(hail1, hail2))
  }

  test("custom") {
    val hail1 = Hail(15, 15, 15, -2, 1, -2)
    val hail2 = Hail(17, 14, 15, -4, 2, -4)
    assert(Day24.intersect2d(hail1, hail1).nonEmpty)
  }
}
