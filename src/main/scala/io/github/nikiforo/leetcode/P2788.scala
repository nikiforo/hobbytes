package io.github.nikiforo.leetcode

object P2788 {

  def splitWordsBySeparator(words: List[String], separator: Char): List[String] =
    words.flatMap(_.split(separator)).filter(_.nonEmpty)
}
