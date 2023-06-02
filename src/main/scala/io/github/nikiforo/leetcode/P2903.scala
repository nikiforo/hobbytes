package io.github.nikiforo.leetcode

object P2903 {

  def findIndices(nums: Array[Int], indexDifference: Int, valueDifference: Int): Array[Int] = {
    val inds =
      for {
        i <- nums.indices
        j <- i until nums.length
        if math.abs(i - j) >= indexDifference && math.abs(nums(i) - nums(j)) >= valueDifference
      } yield (i, j)

    val (v1, v2) = inds.headOption.getOrElse((-1, -1))
    Array(v1, v2)
  }
}
