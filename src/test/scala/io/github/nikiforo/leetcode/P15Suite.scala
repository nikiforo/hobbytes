package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P15Suite extends LeetcodeSuite {

  test("big case") {
    val List(arrString) = resource("P15")
    assert(P15.threeSum(arrString.arint).size == 16258)
  }
}
