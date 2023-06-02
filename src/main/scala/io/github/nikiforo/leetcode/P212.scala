package io.github.nikiforo.leetcode

object P212 {

  private final class Trie {
    private var isTerminal = false
    private val children = scala.collection.mutable.Map.empty[Char, Trie]
    def add(chars: List[Char]): Unit =
      chars match {
        case Nil => isTerminal = true
        case c :: tail => children.getOrElseUpdate(c, new Trie).add(tail)
      }
    def terminal: Boolean = isTerminal
    def next(c: Char): Option[Trie] = children.get(c)
  }

  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    def char(c: (Int, Int)) = board(c._1)(c._2)
    def isBoard(c: (Int, Int)) = (c._1 >= 0 && c._1 < board.length && c._2 >= 0 && c._2 < board(0).length)
    def neighbours(c: (Int, Int)) = List((c._1 + 1, c._2), (c._1 - 1, c._2), (c._1, c._2 + 1), (c._1, c._2 - 1))
    def go(stack: List[((Int, Int), List[(Int, Int)], Trie)], found: List[String]): List[String] =
      stack match {
        case Nil => found
        case (coord, path, trie) :: tail =>
          val nPath = coord :: path
          val nStack =
            for {
              c <- neighbours(coord).filter(isBoard).filter(!path.contains(_))
              t <- trie.next(char(c))
            } yield (c, nPath, t)
          go(nStack ::: tail, if (trie.terminal) nPath.map(char).reverse.mkString :: found else found)
      }

    val wordsTrie = new Trie
    words.foreach(w => wordsTrie.add(w.toList))
    val result =
      for {
        i <- board.indices
        j <- board(i).indices
        t <- wordsTrie.next(char((i, j))).toList
        word <- go(List(((i, j), List.empty, t)), List.empty)
      } yield word
    result.distinct.toList
  }
}
