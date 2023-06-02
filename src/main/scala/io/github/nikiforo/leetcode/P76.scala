package io.github.nikiforo.leetcode

object P76 {

  def minWindow(s: String, t: String): String = {
    val map = t.foldLeft(Map.empty[Char, Int])((m,c) => m.updated(c, m.getOrElse(c, 0) + 1))
    go(None, s.toList, Vector.empty, map)
  }

  private def go(min: Option[Vector[Char]], chars: List[Char], vec: Vector[Char], map: Map[Char, Int]): String =
    if (map.exists(_._2 > 0))
      chars match {
        case Nil => min.fold("")(_.mkString)
        case c :: tail => go(min, tail, vec :+ c, alter(map, c, -1))
      }
    else {
      val nMin = if (vec.length < min.fold(Int.MaxValue)(_.length)) Some(vec) else min
      go(nMin, chars, vec.tail, alter(map, vec.head, 1))
    }
    

  private def alter(map: Map[Char, Int], c: Char, offset: Int): Map[Char, Int] =
    map.get(c) match {
      case None => map
      case Some(value) => map.updated(c, value + offset)
    }
}
