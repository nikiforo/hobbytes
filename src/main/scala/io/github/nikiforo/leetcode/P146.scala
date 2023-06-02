package io.github.nikiforo.leetcode

object P146 {

  class LRUCache(_capacity: Int) {

    private val queue = scala.collection.mutable.Queue.empty[Int]

    private val map = scala.collection.mutable.HashMap.empty[Int, (Int, Int)]

    def get(key: Int): Int =
      map.get(key) match {
        case None => -1
        case Some((v, cnt)) =>
          queue.append(key)
          map.put(key, (v, cnt + 1))
          v
      }

    def put(key: Int, value: Int) {
      queue.append(key)
      map.put(key, (value, map.get(key).fold(1)(_._2 + 1)))
      shrink()
    }

    private def shrink(): Unit =
      while(map.size > _capacity) {
        val k = queue.dequeue()
        val (v, cnt) = map(k)
        if (cnt <= 1) map.remove(k) else map.put(k, (v, cnt - 1))
      }
  }
}
