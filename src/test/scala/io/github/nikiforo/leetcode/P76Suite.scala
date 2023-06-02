package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P76Suite extends LeetcodeSuite {

  test("""s = "ADOBECODEBANC", t = "ABC"""") {
    assert(P76.minWindow(s = "ADOBECODEBANC", t = "ABC") == "BANC")
  }
}
