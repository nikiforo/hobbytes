package io.github.nikiforo.aoc23

object Day14 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("14"))}
         |task2: ${task2(aocLines("14"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]): Int = {
    val cost = lines.size
    lines.map(_.toList).transpose.map(count(_, cost, 0, 0)).sum
  }

  def task2(lines: List[String]) = ""

  private def count(row: List[Char], cost: Int, dots: Int, acc: Int): Int =
    row match {
      case 'O' :: tail => count(tail, cost - 1, dots, acc + cost)
      case '.' :: tail => count(tail, cost, dots + 1, acc)
      case '#' :: tail => count(tail, cost - dots - 1, 0, acc)
      case _ => acc
    }
}
