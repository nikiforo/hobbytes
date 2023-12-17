package io.github.nikiforo.aoc23

object Day12 extends DayApp("12") {

  import scala.collection.mutable

  abstract class TopDownTab[S, O](tab: scala.collection.mutable.Map[S, O]) {

    final def compute(s: S): O = tab.getOrElseUpdate(s, bare(s))

    protected def bare(s: S): O
  }

  type Request = (List[Char], Int, List[Int])

  final class Solver(tab: mutable.Map[Request, Long]) extends TopDownTab(tab) {

    def bare(request: Request): Long = {
      val (chars, damagedGroup, damagedFreshGroups) = request
      chars match {
        case Nil => if (damagedGroup <= 0 && damagedFreshGroups.isEmpty) 1 else 0
        case c :: tail =>
          def startFreshGroup =
            damagedFreshGroups match {
              case Nil => 0
              case i :: damTail => compute(tail, i - 1, damTail)
            }
          def continueGroup = compute(tail, damagedGroup - 1, damagedFreshGroups)

          if (damagedGroup < 0)
            if (c == '.') continueGroup
            else if (c == '#') startFreshGroup
            else continueGroup + startFreshGroup
          else if (damagedGroup == 0)
            if (c == '.') continueGroup
            else if (c == '#') 0
            else continueGroup
          else
            if (c == '.') 0
            else if (c == '#') continueGroup
            else continueGroup
      }
    }
  }

  def task1(lines: List[String]) =
    lines.map { line =>
      val (chars, next) = parse(line)
      val solver = new Solver(mutable.Map.empty)
      solver.compute((chars, -1, next))
    }.sum

  def task2(lines: List[String]) =
    lines.map { case line =>
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
