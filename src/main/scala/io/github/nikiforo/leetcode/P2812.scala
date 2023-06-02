package io.github.nikiforo.leetcode

// TODO: TLE
object P2812 {

  import scala.collection.immutable.TreeSet

  def maximumSafenessFactor(grid: List[List[Int]]): Int = {
    val n = grid.length

    val thieves =
      for {
        i <- grid.indices
        j <- grid(i).indices
        if grid(i)(j) == 1
      } yield (i, j)

    val safeMap = safeGrid(0, thieves.toList, Map.empty, n)
    go(TreeSet((safeMap((0, 0)), 0, 0)), Set.empty, safeMap, n)
  }

  private def safeGrid(
    distance: Int,
    points: List[(Int, Int)],
    visited: Map[(Int, Int), Int],
    n: Int,
  ): Map[(Int,Int), Int] = {
    if (visited.size >= n * n) visited
    else {
      val nVisited = visited ++ points.map(_ -> distance)
      val next = List.newBuilder[(Int, Int)]
      points.foreach { c =>
        manhatten(c._1, c._2).foreach(nc => if (inBorder(nc, n) && !nVisited.contains(nc)) next.addOne(nc))
      }
      safeGrid(distance + 1, next.result(), nVisited, n)
    }
  }

  private def manhatten(i: Int, j: Int) =
    List((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))

  private def inBorder(c: (Int, Int), n: Int) =
    c._1 < n && c._1 >= 0 && c._2 < n && c._2 >= 0

  private def go(paths: TreeSet[(Int, Int, Int)], visited: Set[(Int, Int)], map: Map[(Int, Int), Int], n: Int): Int = {
    val (safety, i, j) = paths.last
    if (visited.contains((i, j))) go(paths.init, visited, map, n)
    else if ((i, j) == (n - 1, n - 1)) safety
    else {
      val nPaths = manhatten(i, j).flatMap(c => map.get(c).map(d => (math.min(d, safety), c._1, c._2))) // .filter(!visited.contains(_))
      go(paths.init ++ nPaths, visited + ((i, j)), map, n)
    }
  }
}
