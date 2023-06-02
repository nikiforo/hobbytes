package io.github.nikiforo.leetcode

object P2744 {

  def maximumNumberOfStringPairs(words: Array[String]): Int = {
    val counts =
      words.indices.map { i =>
        val reversed = words(i).reverse
        words.iterator.drop(i + 1).count(_ == reversed)
      }
    counts.sum
  }
}
