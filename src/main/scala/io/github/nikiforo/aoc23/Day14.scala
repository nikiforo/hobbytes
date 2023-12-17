package io.github.nikiforo.aoc23

object Day14 extends DayApp("14") {

  def task1(lines: List[String]) = {
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

  def task2(lines: List[String]) = unsolved
}
