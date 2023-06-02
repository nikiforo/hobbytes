package io.github.nikiforo.leetcode

object P2855 {

  import scala.collection.immutable.Queue

  def minimumRightShifts(nums: List[Int]): Int =
    nums.reverse match {
      case n1 :: tail => go(n1, Queue.from(tail), 0, nums.size)
      case _ => 0
    }

  private def go(n1: Int, queue: Queue[Int], steps: Int, size: Int): Int =
    if (steps == size) -1
    else if (isSorted(n1, queue)) steps
    else go(queue.head, queue.tail :+ n1, steps + 1, size)

  private def isSorted(n1: Int, q: Queue[Int]): Boolean =
    if (q.isEmpty) true
    else n1 > q.head && isSorted(q.head, q.tail)
}
