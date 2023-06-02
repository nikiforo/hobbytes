package io.github.nikiforo.leetcode

object P2899 {

  private val e = List.empty[Int]

  def lastVisitedIntegers(words: List[String]): List[Int] =
    words.foldLeft((e, e, e)) { case ((stack, prevs, result), word) =>
      (word, prevs) match {
        case ("prev", p :: pTail) => (stack, pTail, p :: result)
        case ("prev", Nil) => (stack, Nil, -1 :: result)
        case (int, _) =>
          val nStack = int.toInt :: stack
          (nStack, nStack, result)
      }
    }._3.reverse
}
