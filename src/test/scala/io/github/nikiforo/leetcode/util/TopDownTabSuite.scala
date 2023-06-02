package io.github.nikiforo.leetcode.util

import io.github.nikiforo.leetcode.LeetcodeSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class TopDownTabSuite extends LeetcodeSuite {

  test("fibonacci 1000") {
    val solver =
      new TopDownTab(scala.collection.mutable.Map.empty[Int, Int]) {
        def bare(s: Int): Int =
          if (s < 1) 0
          else if (s == 1) 1
          else compute(s - 2) + compute(s - 1)
      }

    solver.compute(1000)
  }
}
