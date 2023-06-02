package io.github.nikiforo.leetcode

object P2849 {

  def isReachableAtTime(sx: Int, sy: Int, fx: Int, fy: Int, t: Int): Boolean = {
    val distance = math.max(math.abs(sx - fx), math.abs(sy - fy))
    if (distance == 0) t != 1 else t >= distance
  }
}
