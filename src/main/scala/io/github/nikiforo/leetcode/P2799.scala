package io.github.nikiforo.leetcode

object P2799 {
  def countCompleteSubarrays(nums: Array[Int]): Int = {
    val size = nums.toSet.size

    def go(i: Int, j: Int, map: Map[Int, Int], acc: Int): Int =
      if (i == nums.length) acc
      else if (j < nums.length && map.size < size) go(i, j + 1, map.updated(nums(j), map.getOrElse(nums(j), 0) + 1), acc)
      else {
        val cnt = map(nums(i))
        val nMap = if (cnt <= 1) map.removed(nums(i)) else map.updated(nums(i), cnt - 1)
        val nAcc = if (map.size < size) acc else acc + nums.length - j + 1
        go(i + 1, j, nMap, nAcc)
      }

    go(0, 0, Map.empty, 0)
  }
}
