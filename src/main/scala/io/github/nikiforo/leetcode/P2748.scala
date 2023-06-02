package io.github.nikiforo.leetcode

object P2748 {

  def countBeautifulPairs(nums: Array[Int]): Int = {
    val pairsCntByI =
      nums.indices.map { i =>
        (i + 1 until nums.length).count { j =>
          val stringed = s"${nums(i)}${nums(j)}"
          gcd(stringed.head - '0', stringed.last - '0') == 1
        }
      }
    pairsCntByI.sum
  }

  def gcd(a: Int,b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}
