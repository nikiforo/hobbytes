package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P567Suite extends LeetcodeSuite {

  test("ab, eidbaooo") {
    assert(P567.checkInclusion("ab", "eidbaooo") == true)
  }
}
