package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P218Suite extends LeetcodeSuite {

  test("[[1,2,1],[1,2,2],[1,2,3]]") {
    assert(P218.getSkyline("[[1,2,1],[1,2,2],[1,2,3]]".ararint) == "[[1,3],[2,0]]".lilint)
  }
}
