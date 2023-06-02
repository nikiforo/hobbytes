package io.github.nikiforo.leetcode

object P2750 {

  def numberOfGoodSubarraySplitsFold(nums: Array[Int]): Int = {
    val arr = nums.dropWhile(_ == 0)
    if (arr.isEmpty) 0
    else
      arr.foldLeft((1L, 0)) { case ((cnt, zeros), num) =>
        if (num == 0) (cnt, zeros + 1)
        else ((cnt * (zeros + 1)) % 1000000007, 0)
      }._1.toInt
  }

  def numberOfGoodSubarraySplits(nums: Array[Int]): Int = {
    val list = nums.toList.dropWhile(_ == 0)
    if (list.isEmpty) 0 else go(list, 1, 0)
  }

  def go(nums: List[Int], cnt: Long, zeros: Int): Int =
    nums match {
      case Nil => cnt.toInt
      case 0 :: tail => go(tail, cnt, zeros + 1)
      case _ :: tail => go(tail, (cnt * (zeros + 1)) % 1000000007, 0)
    }
}
