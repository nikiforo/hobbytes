package io.github.nikiforo.leetcode

object P2815 {

  def maxSumNaive(nums: Array[Int]): Int = {
    val maxs = nums.map(_.toString().max)
    val sums = nums.indices.flatMap { i =>
      (i + 1 until nums.length).filter(j => maxs(i) == maxs(j)).map(j => nums(i) + nums(j))
    }
    sums.maxOption.getOrElse(-1)
  }

  def maxSumSorted(nums: Array[Int]): Int =
    nums.groupBy(_.toString().max).foldLeft(-1) { case (max, (_, values)) =>
      if (values.length < 2) max
      else math.max(max, values.sorted.takeRight(2).sum)
    }

  def maxSum(nums: Array[Int]): Int =
    nums.groupBy(_.toString().max).foldLeft(-1) { case (max, (_, values)) =>
      if (values.length < 2) max
      else {
        val (max1, max2) =
          values.foldLeft((Int.MinValue, Int.MinValue)) { case (p @ (mm, m), v) =>
            if (v > mm) (v, mm)
            else if (v > m) (mm, v)
            else p
          }
        math.max(max, max1 + max2)
      }
    }
}
