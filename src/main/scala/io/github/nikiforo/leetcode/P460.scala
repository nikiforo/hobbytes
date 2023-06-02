package io.github.nikiforo.leetcode

object P460 {

  class LFUCache(_capacity: Int) {

    import scala.collection.mutable

    private final class Bucket(var left: Bucket, val freq: Int, var right: Option[Bucket]) {

      private val kvs = mutable.Map.empty[Int, Int]
      private val q = mutable.Queue.empty[Int]

      def remove(key: Int) = kvs.remove(key)

      def nonEmpty = kvs.nonEmpty

      def apply(key: Int) = kvs.apply(key)

      def put(key: Int, value: Int): Unit = {
        q.enqueue(key)
        kvs.put(key, value)
      }

      def lru: Int = {
        val i = q.dequeue()
        if (kvs.contains(i)) i else lru
      }
    }

    private val map = mutable.Map.empty[Int, Bucket]

    private val buckets = new Bucket(null, 0, None)

    def get(key: Int): Int =
      map.get(key) match {
        case None => -1
        case Some(bucket) =>
          val value = bucket(key)
          update(key, value, bucket)
          value
      }

    def put(key: Int, value: Int): Unit =
      map.get(key) match {
        case Some(bucket) => update(key, value, bucket)
        case None =>
          if (map.size >= _capacity) buckets.right.foreach(right => remove(right.lru, right))
          insert(buckets, 1, key, value)
      }

    private def update(key: Int, value: Int, bucket: Bucket): Unit =
      insert(remove(key, bucket), bucket.freq + 1, key, value)

    private def remove(key: Int, bucket: Bucket): Bucket = {
      map.remove(key)
      bucket.remove(key)
      if (bucket.nonEmpty) bucket
      else {
        bucket.left.right = bucket.right
        bucket.left
      }
    }

    private def insert(bucket: Bucket, freq: Int, key: Int, value: Int): Unit = {
      bucket.right match {
        case Some(right) if (right.freq == freq) => right.put(key, value)
        case _ =>
          val nRight = new Bucket(bucket, freq, bucket.right)
          nRight.put(key, value)
          bucket.right.foreach(_.left = nRight)
          bucket.right = Some(nRight)
      }
      map.put(key, bucket.right.get)
    }
  }
}
