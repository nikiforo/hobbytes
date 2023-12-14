package io.github.nikiforo.aoc23

object Day14 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("14"))}
         |task2: ${task2(aocLines("14"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]): Int = {
    val initCost = lines.size
    lines.map(_.toList).transpose.map { row =>
      row.foldLeft((initCost, 0, 0)) { case ((cost, dots, acc), char) =>
        char match {
          case 'O' => (cost - 1, dots, acc + cost)
          case '.' => (cost, dots + 1, acc)
          case _ => (cost - dots - 1, 0, acc)
        }
      }._3  
    }.sum
  }

  def task2(lines: List[String]) = ""
}
