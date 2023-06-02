package io.github.nikiforo.leetcode

object P2876 {

  def countVisitedNodes(edges: List[Int]): Array[Int] = {
    val arr = edges.toArray
    val visited = scala.collection.mutable.Map.empty[Int, Int]
    def go(i: Int, path: List[Int], seen: Set[Int]): Unit =
      visited.get(i) match {
        case Some(len) => path.zipWithIndex.foreach { case (e, i) => visited(e) = len + i + 1 }
        case None =>
          if (seen(i)) {
            val (cycle, prefix) = path.splitAt(path.indexWhere(_ == i) + 1)
            val len = cycle.size
            cycle.foreach(visited(_) = len)
            prefix.zipWithIndex.foreach { case (e, i) => visited(e) = len + i + 1 }
          }
          else go(arr(i), i :: path, seen + i)
      }

    arr.indices.foreach(go(_, List.empty, Set.empty))
    visited.toArray.sortBy(_._1).map(_._2)
  }
}
