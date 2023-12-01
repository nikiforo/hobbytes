package io.github.nikiforo.aoc23

object Day1 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("1"))}
         |task2: ${task2(aocLines("1"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]): Int =
    lines.map(getFirstLastDigits).sum

  def getFirstLastDigits(l: String): Int =
    s"${l.find(_.isDigit).get}${l.findLast(_.isDigit).get}".toInt

  def getFirst(line: String, nums: Map[String, Int]): Int =
    line.toList.tails.map { string =>
      nums.keys.collectFirst { case k if string.startsWith(k) => nums(string.take(k.size).mkString) }
    }.flatten.next()

  def task2(lines: List[String]): Int = {
    val nums = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    val numbers = (nums.zipWithIndex ++ (0 to 9).map(_.toString).zipWithIndex).toMap
    val reversed = numbers.map { case (k,v) => k.reverse -> v }
    lines.map { line =>
      val first = getFirst(line, numbers)
      val last = getFirst(line.reverse, reversed)
      s"$first$last".toInt
    }.sum
  }
}
