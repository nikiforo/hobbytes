package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2850Suite extends LeetcodeSuite {
  
  test("[[1,1,0],[1,1,1],[1,2,1]]") {
    assert(P2850.minimumMoves("[[1,1,0],[1,1,1],[1,2,1]]".ararint) == 3)
  }
}
