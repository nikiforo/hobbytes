package io.github.nikiforo.leetcode

object P2865 {

  def maximumSumOfHeights(maxHeights: List[Int]): Long =
    goMaxSum(Nil, maxHeights, 0)

  def goMaxSum(left: List[Int], right: List[Int], max: Long): Long =
    right match {
      case Nil => max
      case height :: tail =>
        val sums = sumDecreasing(height, left) + sumDecreasing(height, tail) + height
        goMaxSum(height :: left, tail, math.max(max, sums))
    }

  def sumDecreasing(init: Int, heights: List[Int]): Long =
    heights.foldLeft((0L, init)) { case ((sum, prev), height) =>
      val min = math.min(prev, height)
      (sum + min, min)
    }._1
}
