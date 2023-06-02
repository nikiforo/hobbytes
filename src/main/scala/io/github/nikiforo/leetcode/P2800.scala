package io.github.nikiforo.leetcode

object P2800 {

  def minimumString(a: String, b: String, c: String): String =
    List(a, b, c).permutations.map(merge(_).mkString).minBy(s => (s.length, s))

  def merge(strings: List[String]): List[Char] =
    strings.foldLeft(List.empty[Char]) { (acc, string) =>
      if (acc.containsSlice(string)) acc else superpose(string, acc, List.empty)
    }

  def superpose(string: String, check: List[Char], checked: List[Char]): List[Char] =
    if (string.startsWith(check)) checked.reverse ::: string.toList
    else superpose(string, check.tail, check.head :: checked)
}
