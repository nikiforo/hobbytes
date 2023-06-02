package io.github.nikiforo.leetcode

object P567 {

  def checkInclusion(s1: String, s2: String): Boolean = {
    val (left, right) = s2.splitAt(s1.length())
    val added = left.foldLeft(Map.empty[Char, Int])(add)
    go(right.zip(s2).toList, s1.foldLeft(added)(del))
  }

  private def go(chars2: List[(Char, Char)], chars1: Map[Char, Int]): Boolean =
    if(chars1.isEmpty) true
    else 
      chars2 match {
        case Nil => false
        case (ins, rem) :: tail => go(tail, del(add(chars1, ins), rem))
      }
    

  private def add(chars: Map[Char, Int], c: Char) = alterMap(chars, c, 1)

  private def del(chars: Map[Char, Int], c: Char) = alterMap(chars, c, -1)

  private def alterMap(chars: Map[Char, Int], c: Char, offset: Int) = {
    val cnt = chars.getOrElse(c, 0) + offset
    if (cnt == 0) chars.removed(c) else chars.updated(c, cnt)
  }
}
