package io.github.nikiforo.leetcode

object P2924 {

  def findChampion(n: Int, edges: Array[Array[Int]]): Int = {
    val noDefeats = (0 until n).toSet -- edges.map(_(1))
    if (noDefeats.size == 1) noDefeats.head else -1
  }
}
