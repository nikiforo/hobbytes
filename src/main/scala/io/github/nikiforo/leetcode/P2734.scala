package io.github.nikiforo.leetcode

object P2734 {

  def smallestString(s: String): String =
    pre(s.head, s.toList.tail, List.empty).mkString

  private def pre(prev: Char, chars: List[Char], acc: List[Char]): List[Char] =
    (prev, chars) match {
      case ('a', Nil) => ('z' :: acc).reverse
      case ('a', c :: tail) => pre(c, tail, 'a' :: acc)
      case _ => post(prev :: chars, acc)
    }

  private def post(chars: List[Char], acc: List[Char]): List[Char] =
    chars match {
      case Nil => acc.reverse
      case 'a' :: tail => acc.reverse ++ chars
      case c :: tail => post(tail, (c - 1).toChar :: acc)
    }
}
