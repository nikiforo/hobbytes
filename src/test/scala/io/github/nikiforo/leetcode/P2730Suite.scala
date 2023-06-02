package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2730Suite extends LeetcodeSuite {
  
  test("52233") {
    assert(P2730.longestSemiRepetitiveSubstring("52233") == 4)
  }

  test("5494") {
    assert(P2730.longestSemiRepetitiveSubstring("5494") == 4)
  }

  test("1111111") {
    assert(P2730.longestSemiRepetitiveSubstring("1111111") == 2)
  }

  test("0001") {
    assert(P2730.longestSemiRepetitiveSubstring("0001") == 3)
  }
}
