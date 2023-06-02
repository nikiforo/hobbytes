package io.github.nikiforo.leetcode

object P2841 {

  def maxSum(nums: List[Int], m: Int, k: Int): Long = {
    val (init, others) = nums.splitAt(k - 1)
    val initMap = init.foldLeft(Map.empty[Int, Int])(alterMap(_, _, 1))
    val initSum = init.foldLeft(0L)(_ + _)
    others.zip(nums).foldLeft((0L, initSum, initMap)) { case ((max, sum, map), (add, rem)) =>
      val nMap = alterMap(map, add, 1)
      val nSum = sum + add
      (if(nMap.size >= m) math.max(nSum, max) else max, nSum - rem, alterMap(nMap, rem, -1))
    }._1
  }

  private def alterMap(ints: Map[Int, Int], i: Int, offset: Int) = {
    val cnt = ints.getOrElse(i, 0) + offset
    if (cnt == 0) ints.removed(i) else ints.updated(i, cnt)
  }
}
