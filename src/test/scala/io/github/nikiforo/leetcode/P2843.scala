package io.github.nikiforo.leetcode

object P2843 {

  def countSymmetricIntegers(low: Int, high: Int): Int =
    (low to high).count(isSymmetric)

  private def isSymmetric(i: Int) = {
    val s = i.toString()
    val (left, right) = s.splitAt(s.length() / 2)
    (s.length() % 2 == 0) && left.sum == right.sum
  }
}
