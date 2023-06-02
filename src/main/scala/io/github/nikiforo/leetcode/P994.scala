package io.github.nikiforo.leetcode

object P994 {

  def orangesRotting(grid: Array[Array[Int]]): Int = {
    def inBorder(c: (Int, Int)) = c._1 >= 0 && c._1 < grid.length && c._2 >= 0 && c._2 < grid(0).length
    def isType(c: (Int, Int), tpe: Int) = grid(c._1)(c._2) == tpe
    def indicesByType(tpe: Int) = grid.indices.flatMap(i => grid(i).indices.map(j => (i, j)).filter(isType(_, tpe)))
    def countMinutes(rotten: Seq[(Int, Int)], minutes: Int): Int = {
      val fresh = rotten.flatMap(coord => neighbour(coord)).filter(inBorder).filter(isType(_, 1)).distinct
      if (fresh.isEmpty && indicesByType(1).nonEmpty) -1
      else if(fresh.isEmpty) minutes
      else {
        fresh.foreach(c => grid(c._1)(c._2) = 2)
        countMinutes(fresh, minutes + 1)
      }
    }
    countMinutes(indicesByType(2), 0)
  }

  private def neighbour(c: (Int, Int)) = List((c._1 + 1, c._2), (c._1 - 1, c._2), (c._1, c._2 + 1), (c._1, c._2 - 1))
}
