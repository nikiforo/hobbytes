package io.github.nikiforo.leetcode

object P2681 {

  def sumOfPower(nums: Array[Int]): Int =
    nums.sorted.foldLeft((0L, 0L)) { case ((acc, sum), n) => (m(acc + m((sum + n) * n) * n), m(sum * 2 + n)) }._1.toInt

  private def m(a: Long) = a % 1000000007
}
