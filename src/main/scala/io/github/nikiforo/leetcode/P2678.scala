package io.github.nikiforo.leetcode

object P2678 {

  def countSeniors(details: Array[String]): Int =
    details.count(_.drop(11).take(2).toInt > 60)
}
