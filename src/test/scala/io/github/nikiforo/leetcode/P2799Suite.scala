package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2799Suite extends LeetcodeSuite {

  test("[1,3,1,2,2]") {
    assert(P2799.countCompleteSubarrays("[1,3,1,2,2]".arint) == 4)
  }

  test("[5,5]") {
    assert(P2799.countCompleteSubarrays("[5,5]".arint) == 3)
  }

  test("[5,5,5,5]") {
    assert(P2799.countCompleteSubarrays("[5,5,5,5]".arint) == 10)
  }

  test("[459,459,962,1579,1435,756,1872,1597]") {
    assert(P2799.countCompleteSubarrays("[459,459,962,1579,1435,756,1872,1597]".arint) == 2)
  }
}
