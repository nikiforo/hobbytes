package io.github.nikiforo.leetcode

object P2933 {

  import scala.collection.immutable.Queue

  case class Entry(name: String, time: Int)

  private val init = (Set.empty[String], Map.empty[String, Queue[Int]])

  def findHighAccessEmployees(access_times: List[List[String]]): List[String] = {
    val entries = access_times.collect { case List(n, t) => Entry(n, t.take(2).toInt * 60 + t.drop(2).toInt) }
    entries.sortBy(_.time).foldLeft(init) { case (state @ (high, history), entry) =>
      if (high.contains(entry.name)) state
      else {
        val queue = history.getOrElse(entry.name, Queue.empty).dropWhile(_ <= entry.time - 60) :+ entry.time
        if (queue.size < 3) (high, history.updated(entry.name, queue))
        else (high + entry.name, history.removed(entry.name))
      }  
    }._1.toList
  }
}
