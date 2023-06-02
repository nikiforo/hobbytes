package io.github.nikiforo.leetcode

object P2784 {

  def isGood(nums: Array[Int]): Boolean =
    nums.sorted.sameElements((1 until nums.length) :+ nums.length - 1)
}
