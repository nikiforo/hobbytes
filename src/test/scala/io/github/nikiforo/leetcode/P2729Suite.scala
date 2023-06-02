package io.github.nikiforo.leetcode

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2729Suite extends AnyFunSuite {

  test("192") {
    assert(P2729.isFascinating(192) == true)
  }

  test("100") {
    assert(P2729.isFascinating(100) == false)
  }

  test("267") {
    assert(P2729.isFascinating(267) == false)
  }
}
