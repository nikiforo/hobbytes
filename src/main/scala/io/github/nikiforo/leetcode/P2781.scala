package io.github.nikiforo.leetcode

object P2781 {

  private final class Trie {
    private var isTerminal = false
    private val children = scala.collection.mutable.Map.empty[Char, Trie]

    def add(chars: List[Char]): Unit =
      chars match {
        case Nil => isTerminal = true
        case c :: tail => children.getOrElseUpdate(c, new Trie).add(tail)
      }

    def invalidBorder(chars: List[Char]): Option[Int] =
      chars match {
        case Nil => terminal
        case c :: tail => terminal.orElse(children.get(c).flatMap(_.invalidBorder(tail)).map(_ + 1))
      }

    private def terminal = if (isTerminal) Some(0) else None
  }

  def longestValidSubstring(word: String, forbidden: List[String]): Int = {
    val trie = new Trie
    forbidden.foreach(string => trie.add(string.toList))
    go(word.toList, trie, 0, 0)
  }

  private def go(word: List[Char], trie: Trie, valid: Int, maxValid: Int): Int =
    word match {
      case Nil => math.max(valid, maxValid)
      case _ :: tail =>
        trie.invalidBorder(word) match {
          case None => go(tail, trie, valid + 1, maxValid)
          case Some(border) =>
            val mins = word.tails.zipWithIndex.map { case (chars, i) => trie.invalidBorder(chars).map(_ + i) }
            val trieValid = mins.take(border).toList.flatten.min - 1
            go(tail, trie, 0, math.max(valid + trieValid, maxValid))
        }
    }
}
