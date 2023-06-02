package io.github.nikiforo.leetcode

object P417 {

  def pacificAtlantic(heights: Array[Array[Int]]): List[List[Int]] = {
    val (m, n) = (heights.length, heights(0).length)
    val pacificShore = (heights.indices.map(i => (i, 0)) ++ heights(0).indices.map(j => (0, j))).toList
    val atlanticShore = (heights.indices.map(i => (i, n - 1)) ++ heights(0).indices.map(j => (m - 1, j))).toList
    val both = reach(pacificShore, Set.empty, heights).intersect(reach(atlanticShore, Set.empty, heights))
    both.map(c => List(c._1, c._2)).toList
  }

  private def reach(visit: List[(Int, Int)], visited: Set[(Int, Int)], heights: Array[Array[Int]]): Set[(Int, Int)] =
    visit match {
      case Nil => visited
      case (i, j) :: tail =>
        if (visited.contains((i, j))) reach(tail, visited, heights)
        else {
          val close = List((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
          val border = close.filter(c => c._1 >= 0 && c._1 < heights.length && c._2 >= 0 && c._2 < heights(0).length)
          val higher = border.filter(c => heights(c._1)(c._2) >= heights(i)(j))
          reach(higher ::: tail, visited + ((i, j)), heights)
        }
    }
}
