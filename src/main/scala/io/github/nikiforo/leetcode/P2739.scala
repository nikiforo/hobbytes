package io.github.nikiforo.leetcode

object P2739 {

  def distanceTraveled(main: Int, other: Int): Int =
    if (main < 5) main * 10
    else 50 + (if (other > 0) distanceTraveled(main - 4, other - 1) else distanceTraveled(main - 5, 0))
}
