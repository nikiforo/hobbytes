package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2872Suite extends LeetcodeSuite {

  test("n = 5, edges = [[0,2],[1,2],[1,3],[2,4]], values = [1,8,1,4,4], k = 6") {
    val actual = P2872.maxKDivisibleComponents(5, "[[0,2],[1,2],[1,3],[2,4]]".ararint, "[1,8,1,4,4]".arint, 6)
    assert(actual == 2)
  }

  test("n = 7, edges = [[0,1],[0,2],[1,3],[1,4],[2,5],[2,6]], values = [3,0,6,1,5,2,1], k = 3") {
    val edges = "[[0,1],[0,2],[1,3],[1,4],[2,5],[2,6]]".ararint
    val actual = P2872.maxKDivisibleComponents(7, edges, "[3,0,6,1,5,2,1]".arint, 3)
    assert(actual == 3)
  }

  test("n = 1, edges = [], values = [0], k = 1") {
    val actual = P2872.maxKDivisibleComponents(1, "[]".ararint, "[0]".arint, 1)
    assert(actual == 1)
  }

  test("n = 2, edges = [[1, 0]], values = [0, 0], k = 100000000") {
    val actual = P2872.maxKDivisibleComponents(2, "[[1,0]]".ararint, "[0,0]".arint, 100000000)
    assert(actual == 2)
  }

  test("last") {
    val actual = P2872.maxKDivisibleComponents(2, "[[0,1]]".ararint, "[1,2]".arint, 1)
    assert(actual == 2)
  }
}
