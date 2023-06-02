package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P994Suite extends LeetcodeSuite {
  test("[[2,1,1],[1,1,0],[0,1,1]]") {
    assert(P994.orangesRotting("[[2,1,1],[1,1,0],[0,1,1]]".ararint) == 4)
  }
}
