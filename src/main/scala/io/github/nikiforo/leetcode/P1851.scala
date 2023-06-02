package io.github.nikiforo.leetcode

object P1851 {

  import scala.collection.immutable.TreeMap

  sealed trait Event
  case class Begin(at: Int, size: Int) extends Event
  case class End(at: Int, size: Int) extends Event
  case class Query(at: Int, index: Int) extends Event

  implicit val ord: Ordering[Event] =
    Ordering.by {
      case Begin(at, _) => (at, 0)
      case Query(at, _) => (at, 1)
      case End(at, _) => (at, 2)
    }

  def minInterval(intervals: Array[Array[Int]], queries: Array[Int]): Array[Int] = {
    val iEvents = intervals.collect { case Array(begin, end) =>
      val size = end - begin + 1
      Array(Begin(begin, size), End(end, size))
    }
    val qEvents = queries.zipWithIndex.map { case (at, ind) => Query(at, ind) }
    val events: List[Event] = (iEvents.flatten ++ qEvents).toList
    go(events.sorted, TreeMap.empty, List.empty).toArray
  }

  def go(events: List[Event], qs: TreeMap[Int, Int], acc: List[(Int, Int)]): List[Int] =
    events match {
      case Nil => acc.sorted.map(_._2)
      case Begin(_, size) :: tail => go(tail, qs.updated(size, qs.getOrElse(size, 0) + 1), acc)
      case Query(_, index) :: tail => go(tail, qs, (index, qs.headOption.fold(-1)(_._1)) :: acc)
      case End(_, size) :: tail =>
        val sizes = qs(size)
        go(tail, if (sizes <= 1) qs.removed(size) else qs.updated(size, sizes - 1), acc)
    }
}
