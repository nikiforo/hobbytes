package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2835Suite extends LeetcodeSuite {
  test("[2,2] 1") {
    assert(P2835.minOperations("[2,2]".lint, 1) == 1)
  }

  test("[2,2] 3") {
    assert(P2835.minOperations("[2,2]".lint, 3) == 1)
  }

  test("[1,2,8]") {
    assert(P2835.minOperations("[1,2,8]".lint, 7) == 1)
  }

  test("[1,32,1,2], 12") {
    assert(P2835.minOperations("[1,32,1,2]".lint, 12) == 2)
  }

  test("[1,32,1], 35") {
    assert(P2835.minOperations("[1,32,1]".lint, 35) == -1)
  }

  test("big") {
    val string = "[128,1024,1073741824,4194304,268435456,1024,16,1073741824,131072,4,16777216,67108864,16777216,268435456,1073741824,256,16,67108864,1048576,16,4,4194304,1024,16,262144,1048576,1024,128,1073741824,67108864,65536,128,32768,128,32768,8192,256,1024]"
    assert(P2835.minOperations(string.lint, 38) == 1)
  }
}
