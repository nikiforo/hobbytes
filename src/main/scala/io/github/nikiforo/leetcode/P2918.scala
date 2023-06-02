package io.github.nikiforo.leetcode

object P2918 {

  def minSum(nums1: Array[Int], nums2: Array[Int]): Long = {
    val (z1, z2) = (nums1.count(_ == 0), nums2.count(_ == 0))
    val (s1, s2) = (sum(nums1) + z1, sum(nums2) + z2)

    if (s1 < s2 && z1 == 0 || s2 < s1 && z2 == 0) -1
    else math.max(s1, s2)
  }

  def sum(nums: Array[Int]) = nums.foldLeft(0L)(_ + _)
}
