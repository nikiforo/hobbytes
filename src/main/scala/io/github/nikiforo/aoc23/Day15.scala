package io.github.nikiforo.aoc23

object Day15 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("15"))}
         |task2: ${task2(aocLines("15"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) =
    lines.flatMap(_.split(",")).map(_.foldLeft(0)((acc, c) => ((acc + c.toInt) * 17) % 256)).sum

  def task2(lines: List[String]) = ""
}
