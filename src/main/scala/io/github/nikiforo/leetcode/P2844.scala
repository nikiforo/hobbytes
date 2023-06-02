package io.github.nikiforo.leetcode

object P2844 {

  def minimumOperations(num: String): Int =
    go(num.reverse.toList, false, false, 0)

  def go(chars: List[Char], found0: Boolean, found5: Boolean, rm: Int): Int =
    chars match {
      case Nil => rm - (if (found0) 1 else 0)
      case ('2' | '7') :: tail if found5 => rm - 1
      case ('5' | '0') :: tail if found0 => rm - 1
      case '0' :: tail => go(tail, true, found5, rm + 1)
      case '5' :: tail => go(tail, found0, true, rm + 1)
      case _ :: tail => go(tail, found0, found5, rm + 1)
    }
}
