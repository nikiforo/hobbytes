package io.github.nikiforo.leetcode

object P2848 {

  def numberOfPoints(nums: List[List[Int]]): Int =
    nums.collect { case List(start, end) => Set.from(start to end) }.reduce(_ ++ _).size
}
