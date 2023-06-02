package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2855Suite extends LeetcodeSuite {

  test("[3,4,5,1,2]") {
    assert(P2855.minimumRightShifts("[3,4,5,1,2]".lint) == 2)
  }

  test("[2,1,4]") {
    assert(P2855.minimumRightShifts("[2,1,4]".lint) == -1)
  }
}
