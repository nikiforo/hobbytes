package io.github.nikiforo.leetcode

object P2928 {

  def distributeCandies(n: Int, limit: Int): Int = {
    val totals =
      for {
        i <- 0 to limit
        j <- 0 to limit
        k <- 0 to limit
      } yield i + j + k

    totals.count(_ == n)
  }
}
