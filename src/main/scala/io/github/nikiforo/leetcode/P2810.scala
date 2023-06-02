package io.github.nikiforo.leetcode

object P2810 {

  def finalStringList(s: String): String =
    s.foldLeft(List.empty[Char])((acc, c) => if (c == 'i') acc.reverse else c :: acc).reverse.mkString

  import scala.collection.immutable.Queue

  def finalString(s: String): String = {
    val (forward, queue) =
      s.foldLeft((true, Queue.empty[Char])) { case ((fwd, acc), c) =>
        if (c == 'i') (!fwd, acc)
        else (fwd, if (fwd) acc :+ c else c +: acc)
      }

    (if(forward) queue else queue.reverse).mkString
  }
    
}
