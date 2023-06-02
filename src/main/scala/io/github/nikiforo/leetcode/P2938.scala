package io.github.nikiforo.leetcode

object P2938 {

  def minimumSteps(s: String): Long =
    s.dropWhile(_ == '0').foldLeft((0L, 0)) { case ((total, ones), c) =>
      if (c == '0') (total + ones, ones) else (total, ones + 1)
    }._1
}
