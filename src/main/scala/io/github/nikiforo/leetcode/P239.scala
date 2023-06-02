package io.github.nikiforo.leetcode

object P239 {

  import scala.collection.immutable.TreeMap

  import scala.collection.immutable.Queue

  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    val (left, right) = nums.splitAt(k - 1)
    val init = left.foldLeft(TreeMap.empty[Int, Int])(alterMap(_, _, 1))
    nums.zip(right).foldLeft((Queue.empty[Int], init))(slidingFold)._1.toArray
  }

  private def slidingFold(state: (Queue[Int], TreeMap[Int, Int]), el: (Int, Int)) = {
    val ((maxs, window), (rem, add)) = (state, el)
    val nWidnow = alterMap(window, add, 1)
    (maxs :+ nWidnow.lastKey, alterMap(nWidnow, rem, -1))
  }

  private def alterMap(nums: TreeMap[Int, Int], n: Int, diff: Int) = {
    val cnt = nums.getOrElse(n, 0) + diff
    if (cnt == 0) nums.removed(n) else nums.updated(n, cnt)
  }
}
