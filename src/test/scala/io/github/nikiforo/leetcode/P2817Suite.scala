package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2817Suite extends LeetcodeSuite {

  test("nums = [4,3,2,4], x = 2") {
    assert(P2817.minAbsoluteDifference(nums = "[4,3,2,4]".lint, x = 2) == 0)
  }
}
