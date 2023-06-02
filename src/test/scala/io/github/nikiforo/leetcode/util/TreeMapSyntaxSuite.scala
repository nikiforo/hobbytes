package io.github.nikiforo.leetcode.util

import io.github.nikiforo.leetcode.LeetcodeSuite
import io.github.nikiforo.leetcode.util.TreeMapSyntax._
import org.scalatest.DoNotDiscover

import scala.collection.immutable.TreeMap

@DoNotDiscover
class TreeMapSyntaxSuite extends LeetcodeSuite {

  test("[1, 3, 1]") {
    val dequeued0 = TreeMap.empty[Int, Int].enqueue(1).enqueue(3).enqueue(1)
    val (d1, dequeued1) = dequeued0.dequeue
    val someDequeue = dequeued0.dequeueOption
    val (d2, dequeued2) = dequeued1.dequeue
    val (d3, dequeued3) = dequeued2.dequeue
    val empty = dequeued3.dequeueOption
    assert((d1, someDequeue, d2, d3, empty) == (1, Some(1, dequeued1), 1, 3, None))
  }
}
