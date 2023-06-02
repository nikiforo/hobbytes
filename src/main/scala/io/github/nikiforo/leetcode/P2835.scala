package io.github.nikiforo.leetcode

object P2835 {

  import scala.collection.immutable.TreeMap

  // BEGIN: TreeMap Queue library extension  

  implicit class TreeMapQueue[K](val counts: TreeMap[K, Int]) extends AnyVal {

    def dequeueOption: Option[(K, TreeMap[K, Int])] = counts.headOption.map(deq)

    def dequeue: (K, TreeMap[K, Int]) = deq(counts.head)

    def enqueue(k: K): TreeMap[K, Int] = counts.updated(k, counts.get(k).getOrElse(0) + 1)

    private def deq(entry: (K, Int)) = {
      val (k, v) = entry
      if (v <= 1) (k, counts.removed(k))
      else (k, counts.updated(k, v - 1))
    }
  }

  // END

  private val empty = TreeMap.empty[Int, Int](Ordering[Int].reverse)

  def minOperations(nums: List[Int], target: Int): Int = {
    val sum = nums.foldLeft(0L)(_ + _)
    val queue = nums.foldLeft(empty)(_.enqueue(_))
    if (sum < target) -1 else count(sum, target, queue, 0)
  }

  private def count(sum: Long, target: Int, queue: TreeMap[Int, Int], cnt: Int): Int =
    if (target <= 0) cnt
    else {
      val (head, tail) = queue.dequeue
      if (sum - head >= target) count(sum - head, target, tail, cnt)
      else if (head <= target) count(sum - head, target - head, tail, cnt)
      else count(sum, target, tail.enqueue(head / 2).enqueue(head / 2), cnt + 1)
    }
}
