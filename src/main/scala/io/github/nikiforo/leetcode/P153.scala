package io.github.nikiforo.leetcode

object P153 {

  def findMin(nums: Array[Int]): Int = {
    def go(l: Int, r: Int): Int = {
      val m = (l + r) / 2
      if (l == r) nums(l)
      else if (nums(m) < nums(r)) go(l, m)
      else go(m + 1, r) 
    }
    go(0, nums.length - 1)
  }
}
