package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2876Suite extends LeetcodeSuite {

  test("big") {
    val List(line) = resource("P2876")
    assertResultTime(60849)(P2876.countVisitedNodes(line.lint).length)
  }

  test("[1,2,0,0]") {
    assert(P2876.countVisitedNodes("[1,2,0,0]".lint).toList == "[3,3,3,4]".lint)
  }

  test("[1,2,3,4,0]") {
    assert(P2876.countVisitedNodes("[1,2,3,4,0]".lint).toList == "[5,5,5,5,5]".lint)
  }
}
