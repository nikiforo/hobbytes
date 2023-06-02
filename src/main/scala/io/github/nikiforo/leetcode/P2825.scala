package io.github.nikiforo.leetcode

object P2825 {

  def canMakeSubsequence(str1: String, str2: String): Boolean =
    go(str1.toList, str2.toList)

  def go(chars: List[Char], pattern: List[Char]): Boolean =
    (chars, pattern) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (c :: cTail, p :: pTail) =>
        if (c == p || cycle(c) == p) go(cTail, pTail)
        else go(cTail, pattern)
    }

  private def cycle(c: Char): Char =
    if (c == 'z') 'a' else (c + 1).toChar
}
