package io.github.nikiforo.aoc23

object Day12 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("12"))}
         |task2: ${task2(aocLines("12"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) =
    lines.map { line =>
      val (chars, next) = parse(line)
      count(chars, -1, next)
    }.sum

  def task2(lines: List[String]) = ""

  def count(chars: List[Char], continue: Int, next: List[Int]): Int =
    chars match {
      case Nil => if (continue <= 0 && next.isEmpty) 1 else 0
      case h :: tail =>
        def broken =
          next match {
            case Nil => 0
            case i :: iTail => count(tail, i - 1, iTail)
          }
        def cont = count(tail, continue - 1, next)

        if (continue < 0)
          if (h == '.') cont
          else if (h == '#') broken
          else cont + broken
        else if (continue == 0)
          if (h == '.') cont
          else if (h == '#') 0
          else cont
        else
          if (h == '.') 0
          else if (h == '#') cont
          else cont
    }

  def parse(line: String) =
    line match { case s"$record $other" => (record.toList, other.split(",").map(_.toInt).toList) }
}
