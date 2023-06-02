package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P679Suite extends LeetcodeSuite {

  test("[1,9,1,2]") {
    assert(P679.judgePoint24("[1,9,1,2]".arint) == true)
  }

  test("[1,2,1,2]") {
    assert(P679.judgePoint24("[1,2,1,2]".arint) == false)
  }
}
