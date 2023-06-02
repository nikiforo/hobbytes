package io.github.nikiforo.leetcode

object P2829 {

  def minimumSum(n: Int, k: Int): Int =
    go(1, Set.empty, k).take(n).sum

  def go(i: Int, wrongSet: Set[Int], k: Int): Iterator[Int] =
    if (wrongSet.contains(i)) go(i + 1, wrongSet, k)
    else Iterator(i) ++ go(i + 1, wrongSet + (k - i), k)
}
