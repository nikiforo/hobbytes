package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2865Suite extends LeetcodeSuite {

  test("[1000000000,1000000000,1000000000]") {
    assert(P2865.maximumSumOfHeights("[1000000000,1000000000,1000000000]".lint) == 3000000000L)
  }
}
