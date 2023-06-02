package io.github.nikiforo.leetcode

object P2869 {

  def minOperations(nums: List[Int], k: Int): Int =
    goMin(nums.reverse, (1 to k).toSet, 0)

  def goMin(nums: List[Int], set: Set[Int], steps: Int): Int =
    if (set.isEmpty) steps
    else goMin(nums.tail, set - nums.head, steps + 1)
}
