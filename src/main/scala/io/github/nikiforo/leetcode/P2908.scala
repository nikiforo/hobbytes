package io.github.nikiforo.leetcode

object P2908 {

  def minimumSum(nums: Array[Int]): Int = {
    val sums =
      for {
        i <- nums.indices
        j <- (i + 1) until nums.length
        k <- (j + 1) until nums.length
        if nums(i) < nums(j) && nums(k) < nums(j)
      } yield nums(i) + nums(j) + nums(k)

    sums.minOption.getOrElse(-1)
  }
}
