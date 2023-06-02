package io.github.nikiforo.leetcode

object P42 {

  def trap(height: Array[Int]): Int = {
    val prefix = height.scanLeft(0)((max, h) => math.max(max, h)).drop(1)
    val postfix = height.scanRight(0)((h, max) => math.max(max, h)).dropRight(1)
    val traps = height.indices.tail.dropRight(1).map { i =>
      val wall = math.min(prefix(i -1), postfix(i + 1))
      math.max(0, wall - height(i))
    }
    traps.sum
  }
}
