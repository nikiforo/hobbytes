package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2748Suite extends LeetcodeSuite {

  test("[2,5,1,4]") {
    assert(P2748.countBeautifulPairs("[2,5,1,4]".arint) == 5)
  }

  test("[11,21,12]") {
    assert(P2748.countBeautifulPairs("[11,21,12]".arint) == 2)
  }
}
