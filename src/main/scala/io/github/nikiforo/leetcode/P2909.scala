package io.github.nikiforo.leetcode

object P2909 {

  // def minimumSum(nums: Array[Int]): Int = {
  //   val minLeft = nums.scanLeft(Int.MaxValue)(_ min _).drop(1)
  //   val minRight = nums.scanRight(Int.MaxValue)(_ min _).dropRight(1)
  //   val triplets = minLeft.zip(nums.zip(minRight.tail).tail)
  //   val peaks = triplets.flatMap { case (l, (n, r)) => if(l < n && r < n) Array(l + n + r) else Array.empty[Int] }
  //   peaks.minOption.getOrElse(-1)
  // }

  def minimumSum(nums: Array[Int]): Int = {
    val minLeft = nums.scanLeft(Int.MaxValue)(_ min _).drop(1)
    val minRight = nums.scanRight(Int.MaxValue)(_ min _).dropRight(1)
    val peaks = (1 until (nums.length - 1)).flatMap { j =>
      val (l, n, r) = (minLeft(j - 1), nums(j), minRight(j + 1))
      if(l < n && r < n) Some(l + n + r) else None
    }
    peaks.minOption.getOrElse(-1)
  }
}
