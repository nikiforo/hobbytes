package io.github.nikiforo.leetcode

object P2913 {

  def sumCounts(nums: List[Int]): Int = {
    val squares =
      for {
        i <- nums.indices
        j <- i.until(nums.length)
        distinct = (i to j).map(nums).toSet.size
      } yield distinct * distinct
    squares.sum
  }
}
