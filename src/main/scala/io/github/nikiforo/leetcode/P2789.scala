package io.github.nikiforo.leetcode

object P2789 {
  def maxArrayValue(nums: Array[Int]): Long =
    nums.reverse.foldLeft(0L)((max, num) => if (num > max) num else max + num)
}
