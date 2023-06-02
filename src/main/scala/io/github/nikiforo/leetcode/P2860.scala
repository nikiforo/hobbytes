package io.github.nikiforo.leetcode

object P2860 {

  def countWays(nums: List[Int]): Int = {
    val sorted = nums.sorted
    val zipped = sorted.zip((sorted :+ Int.MaxValue).zipWithIndex.tail)
    (if (nums.forall(_ > 0)) 1 else 0) + zipped.count { case (l, (r, i)) => l < i && i < r }
  }
}
