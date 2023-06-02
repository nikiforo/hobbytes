package io.github.nikiforo.leetcode

object P2824 {

  def countPairs(nums: List[Int], target: Int): Int =
    nums match {
      case num :: tail => tail.count(_ + num < target) + countPairs(tail, target)
      case Nil => 0
    }
}
