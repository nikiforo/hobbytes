package io.github.nikiforo.aoc23

object Day6 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("6"))}
         |task2: ${task2(aocLines("6"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) = {
    val (times, distances) = (parse1(lines(0)), parse1(lines(1)))
    times.zip(distances).map { case (time, distance) => (1 until time).count(t => t * (time - t) > distance) }.product
  }

  def task2(lines: List[String]) = {
    val (time, distance) = (parse2(lines(0)), parse2(lines(1)))
    (1L until time).count(t => t * (time - t) > distance)
  } 

  private def parse1(line: String) =
    line.split(" +").toList.tail.map(_.toInt)

  private def parse2(line: String) =
    line.split(" +").toList.tail.mkString.toLong
}
