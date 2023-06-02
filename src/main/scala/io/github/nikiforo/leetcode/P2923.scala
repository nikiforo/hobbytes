package io.github.nikiforo.leetcode

object P2923 {

  def findChampion(grid: Array[Array[Int]]): Int =
    grid.indexWhere(_.sum == grid.length - 1)
}
