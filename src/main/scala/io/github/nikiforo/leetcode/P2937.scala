package io.github.nikiforo.leetcode

object P2937 {

  def findMinimumOperations(s1: String, s2: String, s3: String): Int = {
    val min = s1.zip(s2).zip(s3).takeWhile { case ((c1, c2), c3) => c1 == c2 && c2 == c3 }.size
    if (min == 0) -1 else List(s1, s2, s3).map(_.size - min).sum
  }
}
