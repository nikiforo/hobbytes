package io.github.nikiforo.leetcode

object P2817 {

  import scala.collection.immutable.TreeSet

  def minAbsoluteDifference(nums: List[Int], x: Int): Int =
    nums.zip(nums.drop(x)).foldLeft((TreeSet.empty[Int], Int.MaxValue))(evalFold)._2

  private def evalFold(state: (TreeSet[Int], Int), elem: (Int, Int)) = {
    val ((set, min), (left, right)) = (state, elem)
    val nSet = set + left
    val diffs = List(nSet.maxBefore(right), nSet.minAfter(right)).flatten.map(n => math.abs(right - n))
    (nSet, (min :: diffs).min)
  }
}
