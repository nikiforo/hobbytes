package io.github.nikiforo.aoc23

abstract class DayApp(filename: String) {

  protected val unsolved = -1L

  final def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines(filename))}
         |task2: ${task2(aocLines(filename))}""".stripMargin
    println(result)
  }

  protected def task1(lines: List[String]): Long

  protected def task2(lines: List[String]): Long
}
