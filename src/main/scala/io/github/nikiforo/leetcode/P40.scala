package io.github.nikiforo.leetcode

import scala.collection.immutable

object P40 {

  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    val picked = List.newBuilder[List[Int]]
    def go(p: Int, pTail: List[Int], cands: List[Int], sum: Int): Unit =
      if (sum == target) picked.addOne(p :: pTail)
      else if(sum < target && cands.nonEmpty) {
        go(cands.head, p :: pTail, cands.tail, sum + cands.head)
        val replace = cands.dropWhile(_ == p)
        if(replace.nonEmpty) go(replace.head, pTail, replace.tail, sum - p + replace.head)
      }
    val sorted = candidates.sorted.toList
    go(sorted.head, Nil, sorted.tail, sorted.head)
    picked.result()
  }
}
