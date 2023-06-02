package io.github.nikiforo.leetcode

object P2859 {

  def sumIndicesWithKSetBitsCollect(nums: List[Int], k: Int): Int =
    nums.zipWithIndex.collect { case (n, i) if i.toBinaryString.count(_ == '1') == k => n }.sum

  def sumIndicesWithKSetBits(nums: List[Int], k: Int): Int =
    nums.zipWithIndex.foldLeft(0) { case (s, (n, i)) => s + (if(i.toBinaryString.count(_ == '1') == k) n else 0) }
}
