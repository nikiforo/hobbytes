package io.github.nikiforo.leetcode

object P2864 {

  def maximumOddBinaryNumber(s: String): String = {
    val ones = s.count(_ == '1')
    "1" * (ones - 1) + "0" * (s.length() - ones) + "1"
  }
}
