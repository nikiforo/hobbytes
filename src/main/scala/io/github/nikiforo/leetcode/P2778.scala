package io.github.nikiforo.leetcode

object P2778 {
  def sumOfSquares(nums: Array[Int]): Int =
    nums.zipWithIndex.map { case (num, i) => if (nums.length % (i + 1) == 0) num * num else 0 }.sum
}
