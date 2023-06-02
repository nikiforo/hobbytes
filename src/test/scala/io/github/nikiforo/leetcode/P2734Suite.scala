package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2734Suite extends LeetcodeSuite {

  test("cbabc") {
    assert(P2734.smallestString("cbabc") == "baabc")
  }

  test("acbbc") {
    assert(P2734.smallestString("acbbc") == "abaab")
  }

  test("a") {
    assert(P2734.smallestString("a") == "z")
  }
}
