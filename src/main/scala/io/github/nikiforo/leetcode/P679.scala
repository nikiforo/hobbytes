package io.github.nikiforo.leetcode

object P679 {

  def judgePoint24(cards: Array[Int]): Boolean =
    go(cards.map(_.toFloat).toList)

  def go(cards: List[Float]): Boolean =
    if (cards.size == 1) math.abs(cards.head - 24) < 0.001
    else
      (0 until cards.size - 1).exists { i =>
        (i + 1 until cards.size).exists { j =>
          val next = cards.take(i) ::: cards.slice(i + 1, j) ::: cards.drop(j + 1)
          val (c1, c2) = (cards(i), cards(j))
          List(c1 - c2, c2 - c1, c1 / c2, c2 / c1, c1 + c2, c1 * c2).exists(num => go(num :: next))
        }
      }
}
