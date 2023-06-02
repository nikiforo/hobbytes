package io.github.nikiforo.leetcode

object P2833 {
  
  def furthestDistanceFromOrigin(moves: String): Int = {
    val (left, right) = moves.foldLeft((0, 0)) { case ((l, r), el) =>
      if (el == 'L') (l - 1, r - 1)
      else if (el == 'R') (l + 1, r + 1)
      else (l - 1, r + 1)
    }

    math.max(math.abs(left), math.abs(right))
  }
}
