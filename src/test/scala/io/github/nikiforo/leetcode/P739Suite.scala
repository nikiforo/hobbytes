package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P739Suite extends LeetcodeSuite {

  private def last(i: Int) = (i, 0)

  test("insert 5, 4, 6, 3") {
    val stack1 = P739.insert(3, 1, List.empty)
    val stack2 = P739.insert(6, 1, stack1)
    val stack3 = P739.insert(4, 1, stack2)
    val stack4 = P739.insert(5, 1, stack3)

    assert(stack1 == List(last(3)))
    assert(stack2 == List(last(6)))
    assert(stack3 == List((4, 1), last(6)))
    assert(stack4 == List((5, 2), last(6)))
  }

  test("[73,74,75,71,69,72,76,73]") {
    assert(P739.dailyTemperatures("[73,74,75,71,69,72,76,73]".arint).toList == "[1,1,4,2,1,1,0,0]".lint)
  }
}
