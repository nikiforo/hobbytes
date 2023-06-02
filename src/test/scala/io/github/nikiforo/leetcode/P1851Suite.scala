package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P1851Suite extends LeetcodeSuite {

  test("big case") {
    val List(intervals, queries) = resource("P1851")
    println(intervals.ararint.length)
    assertResultTime(100000)(P1851.minInterval(intervals.ararint, queries.arint).size)
  }
}

