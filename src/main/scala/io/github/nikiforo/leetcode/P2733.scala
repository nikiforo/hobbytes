package io.github.nikiforo.leetcode

object P2733 {

  def findNonMinOrMax(nums: Array[Int]): Int =
    if(nums.length < 3) -1
    else {
      val (min, max) = (nums.min, nums.max)
      nums.find(i => i != min && i != max).get
    }
}
