package io.github.nikiforo.leetcode

object P2828 {

  def isAcronym(words: List[String], s: String): Boolean =
    words.map(_.head).mkString == s
}
