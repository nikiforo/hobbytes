package io.github.nikiforo.aoc23

import scala.io.Source

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

  final protected def timeouted(l: => Long) = -2L

  private def aocLines(file: String): List[String] = {
    val source = Source.fromFile(s"./src/main/resources/aoc/$file")
    val lines = source.getLines.toList
    source.close()
    lines
  }
}
