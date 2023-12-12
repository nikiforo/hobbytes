package io.github.nikiforo.aoc23

object Day12 {

  import scala.collection.mutable

  abstract class TopDownTab[S, O](tab: mutable.Map[S, O]) {

    final def compute(s: S): O = 
      tab.get(s).getOrElse {
        val result = bare(s)
        tab(s) = result
        result
      }

    protected def bare(s: S): O
  }

  type Request = (List[Char], Int, List[Int])

  final class Solver(tab: mutable.Map[Request, Long]) extends TopDownTab(tab) {

    def bare(request: Request): Long = {
      val (chars, continue, next) = request
      chars match {
        case Nil => if (continue <= 0 && next.isEmpty) 1 else 0
        case h :: tail =>
          def broken =
            next match {
              case Nil => 0
              case i :: iTail => compute(tail, i - 1, iTail)
            }
          def cont = compute(tail, continue - 1, next)

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
    }
  }

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("12"))}
         |task2: ${task2(aocLines("12"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) =
    lines.map { line =>
      val (chars, next) = parse(line)
      val solver = new Solver(mutable.Map.empty)
      solver.compute((chars, -1, next))
    }.sum

  def task2(lines: List[String]) =
    lines.zipWithIndex.map { case (line, ind) =>
      val (chars, next) = parse2(line)
      val solver = new Solver(mutable.Map.empty)
      solver.compute((chars, -1, next))
    }.sum

  def parse(line: String) =
    line match { case s"$record $other" => (record.toList, other.split(",").map(_.toInt).toList) }

  def parse2(line: String) = {
    line match { case s"$record $other" => (expand(record, "?").toList, expand(other, ",").split(",").map(_.toInt).toList) }
  }

  def expand(string: String, sep: String) = (1 to 5).map(_ => string).mkString(sep)
}
