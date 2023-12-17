package io.github.nikiforo.aoc23

object Day15 extends DayApp("15") {

  def task1(lines: List[String]) =
    lines.flatMap(_.split(",")).map(_.foldLeft(0)((acc, c) => ((acc + c.toInt) * 17) % 256)).sum

  def task2(lines: List[String]) = unsolved
}
