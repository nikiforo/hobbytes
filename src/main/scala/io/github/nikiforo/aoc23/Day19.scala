package io.github.nikiforo.aoc23

import scala.collection.parallel.CollectionConverters._

object Day19 extends DayApp("19") {

  private case class Range(left: Long, right: Long) {
    def isEmpty = right < left
    def size = right - left + 1
    def inverted = Range(-right, -left)
  }

  private case class Part(x: Range, m: Range, a: Range, s: Range) {
    def isEmpty = x.isEmpty || m.isEmpty || a.isEmpty || s.isEmpty
    def size = x.size * m.size * a.size * s.size
    def inverted = Part(x.inverted, m.inverted, a.inverted, s.inverted)

    def get(string: String): Range =
      string match {
        case "x" => x
        case "m" => m
        case "a" => a
        case _ => s
      }

    def set(string: String, range: Range): Part =
      string match {
        case "x" => copy(x = range)
        case "m" => copy(m = range)
        case "a" => copy(a = range)
        case _ => copy(s = range)
      }
  }
  
  private case class RuleResult(nextRule: Option[(String, Part)], next: Option[Part], accept: Option[Part])

  private type Rule = Part => RuleResult

  def task1(lines: List[String]): Long =
    unsolved

  def task2(lines: List[String]): Long = {
    val r = Range(1, 4000)
    val workflows = parse(lines)
    flow(workflows, Part(r, r, r, r), workflows("in"))
  }

  private def flow(map: Map[String, List[Rule]], part: Part, rules: List[Rule]): Long = {
    val result = rules.head(part)
    val otherCnt = result.nextRule.fold(0L) { case (s, p) => flow(map, p, map(s)) }
    val nextCnt = result.next.fold(0L)(p => flow(map, p, rules.tail))
    otherCnt + nextCnt + result.accept.fold(0L)(_.size)
  }

  private def parse(lines: List[String]) = {
    val ind = lines.indexWhere(_.isEmpty())
    val (workflowStrings, partStrings) = (lines.take(ind), lines.drop(ind + 1))
    val workflows = workflowStrings.map { case s"$name{$rules}" => name -> rules.split(",").map(parseRule).toList }
    workflows.toMap
  }

  private def parseRule(string: String): Rule =
    part => {
      val ((miss, applied), outcomeString) =
        string match {
          case s"$prop<$num:$outcome" => (split(prop, num.toInt, part), outcome)
          case s"$prop>$num:$outcome" =>
            val (l, r) = split(prop, -num.toInt, part.inverted)
            ((l.map(_.inverted), r.map(_.inverted)), outcome)
          case outcome => ((None, Some(part)), outcome)
        }
      outcomeString match {
        case "A" => RuleResult(None, miss, applied)
        case "R" => RuleResult(None, miss, None)
        case other => RuleResult(applied.map((other, _)), miss, None)
      }
    }

  private def split(prop: String, num: Int, part: Part) = {
    val (left, right) = lt(num.toInt, part.get(prop))
    (left.map(part.set(prop, _)), right.map(part.set(prop, _)))
  }

  private def lt(point: Int, range: Range): (Option[Range], Option[Range]) =
    if (range.left >= point) (Some(range), None)
    else if (range.right < point) (None, Some(range))
    else (Some(Range(point, range.right)), Some(Range(range.left, point - 1)))

  private def parseOutcome(outcome: String) =
    outcome match {
      case "A" => Right(true)
      case "R" => Right(false)
      case other => Left(other)
    }
}
