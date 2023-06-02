package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2826Suite extends LeetcodeSuite {

  test("[2,1,3,2,1]") {
    assert(P2826.minimumOperations("[2,1,3,2,1]".lint) == 3)
  }

  test("[1]") {
    assert(P2826.minimumOperations("[1]".lint) == 0)
  }
}
