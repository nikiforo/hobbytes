package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2844Suite extends LeetcodeSuite {

  test("2245047") {
    assert(P2844.minimumOperations("2245047") == 2)
  }

  test("2908305") {
    assert(P2844.minimumOperations("2908305") == 3)
  }

  test("10") {
    assert(P2844.minimumOperations("10") == 1)
  }

  test("5") {
    assert(P2844.minimumOperations("5") == 1)
  }
}
