package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2762Suite extends LeetcodeSuite {

  test("[5,4,2,4]") {
    assert(P2762.continuousSubarrays("[5,4,2,4]".arint) == 8)
  }

  test("big") {
    val List(line) = resource("P2762")
    assertResultTime(5000050000L)(P2762.continuousSubarrays(line.arint))
  }
}
