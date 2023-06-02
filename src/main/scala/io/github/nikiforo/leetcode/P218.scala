package io.github.nikiforo.leetcode

object P218 {

  import scala.collection.immutable.TreeMap

  sealed trait Event
  case class Begin(x: Int, height: Int) extends Event
  case class End(x: Int, height: Int) extends Event

  implicit val ord: Ordering[Event] =
    Ordering.by {
      case Begin(x, height) => (x, 0, -height)
      case End(x, height) => (x, 1, height)
    }

  def getSkyline(buildings: Array[Array[Int]]): List[List[Int]] = {
    val events: Array[Event] = buildings.collect { case Array(l, r, h) => Array(Begin(l, h), End(r, h)) }.flatten
    go(events.sorted.toList, TreeMap.empty, List.empty)
  }

  private def go(events: List[Event], heights: TreeMap[Int, Int], acc: List[List[Int]]): List[List[Int]] =
    events match {
      case Nil => acc.reverse
      case Begin(x, height) :: tail =>
        val nAcc = if (max(heights) < height) List(x, height) :: acc else acc
        go(tail, heights.updated(height, heights.getOrElse(height, 0) + 1), nAcc)
      case End(x, height) :: tail =>
        val cnt = heights(height)
        val nHeights = if (cnt <= 1) heights.removed(height) else heights.updated(height, cnt - 1)
        val nAcc = if (cnt <= 1 && height == max(heights)) List(x, max(nHeights)) :: acc else acc
        go(tail, nHeights, nAcc)
    }

  private def max(heights: TreeMap[Int, Int]) = 
    heights.lastOption.fold(0)(_._1)
}
