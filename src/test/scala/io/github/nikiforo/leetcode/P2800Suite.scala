package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2800Suite extends LeetcodeSuite {

  test("1") {
    assert(P2800.minimumString("abc", "bca", "aaa") == "aaabca")
  }

  test("2") {
    assert(P2800.minimumString("ab", "ba", "aba") == "aba")
  }

  test(""""cab" "a" "b"""") {
    assert(P2800.minimumString("cab", "a", "b") == "cab")
  }
}
