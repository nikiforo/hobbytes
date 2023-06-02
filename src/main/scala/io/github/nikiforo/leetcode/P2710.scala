package io.github.nikiforo.leetcode

object P2710 {
  def removeTrailingZeros(num: String): String =
    num.reverse.dropWhile(_ == '0').reverse
}
