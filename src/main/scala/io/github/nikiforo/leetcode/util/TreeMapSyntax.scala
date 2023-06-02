package io.github.nikiforo.leetcode.util

object TreeMapSyntax {

  import scala.collection.immutable.TreeMap

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
}
