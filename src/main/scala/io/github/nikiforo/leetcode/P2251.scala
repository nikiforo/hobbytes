package io.github.nikiforo.leetcode

object P2251 {

  sealed trait Event { def at: Int }
  case class Bloom(at: Int) extends Event
  case class Fade(at: Int) extends Event
  case class Visitor(at: Int, id: Int) extends Event

  implicit val ord: Ordering[Event] =
    Ordering.by {
      case Bloom(at) => (at, 0)
      case Visitor(at, _) => (at, 1)
      case Fade(at) => (at, 2)
    }

  def fullBloomFlowers(flowers: Array[Array[Int]], people: Array[Int]): Array[Int] = {
    val flowerEvents = flowers.collect { case Array(bloomAt, fadeAt) => Array(Bloom(bloomAt), Fade(fadeAt)) }
    val events: Array[Event] = flowerEvents.flatten ++ people.zipWithIndex.map { case (at, id) => Visitor(at, id) }
    go(events.sorted.toList, 0, List.empty).toArray
  }

  def go(events: List[Event], cnt: Int, acc: List[(Int, Int)]): List[Int] =
    events match {
      case Nil => acc.sorted.map(_._2)
      case Bloom(_) :: tail => go(tail, cnt + 1, acc)
      case Fade(_) :: tail => go(tail, cnt - 1, acc)
      case Visitor(_, id) :: tail => go(tail, cnt, (id, cnt) :: acc)
    }
}
