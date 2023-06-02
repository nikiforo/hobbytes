package io.github.nikiforo.leetcode

object P2873 {

  def maximumTripletValue(nums: Array[Int]): Long = {
    val triplets =
      for {
        i <- nums.indices
        j <- (i + 1) until nums.length
        k <- (j + 1) until nums.length
      } yield (nums(i).toLong - nums(j)) * nums(k)

    math.max(0, triplets.max)
  }
}
