package io.github.nikiforo.leetcode

object P424 {

  import scala.collection.immutable.Queue

  def characterReplacement(s: String, k: Int): Int =
    go(s.toList.tail, k, Queue(s.head), Map(s.head -> 1), 1)

  def go(word: List[Char], k: Int, q: Queue[Char], map: Map[Char, Int], max: Int): Int = {
    val size = map.values.sum
    if (size - map.values.max > k) go(word, k, q.tail, map.updated(q.head, map(q.head) - 1), max)
    else
      word match {
        case Nil => math.max(max, size)
        case c :: tail => go(tail, k, q :+ c, map.updated(c, map.getOrElse(c, 0) + 1), math.max(max, size))
      }
  }
    
      
}
