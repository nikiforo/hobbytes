package io.github.nikiforo.leetcode

object P2729 {

  private val fascinatingString = (1 to 9).mkString

  def isFascinating(n: Int) =
    s"$n${n * 2}${n * 3}".sorted == fascinatingString
}
