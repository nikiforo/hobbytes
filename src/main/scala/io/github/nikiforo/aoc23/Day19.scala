package io.github.nikiforo.aoc23

import scala.collection.parallel.CollectionConverters._

object Day19 extends DayApp("19") {

  case class Part(x: Int, m: Int, a: Int, s: Int)

  def getter(s: String, part: Part): Int =
    s match {
      case "x" => part.x
      case "m" => part.m
      case "a" => part.a
      case _ => part.s
    }

  type Rule = Part => Option[Either[String, Boolean]]

  def task1(lines: List[String]): Long = {
    val (workflows, parts) = parse(lines)
    parts.filter(flow(workflows, _, workflows("in"))).map(p => p.x + p.m + p.a + p.s).sum
  }

  //❗️ won't solve the problem
  def task2(lines: List[String]): Long = {
    val (workflows, _) = parse(lines)

    val poss = (1 to 4000).view

    poss.grouped(334).toVector.par.map { xs =>
      val combs =
        for {
          x <- xs
          m <- poss
          a <- poss
          s <- poss
        } yield Part(x, m, a, s)

      combs.foldLeft(0L)((acc, p) => if (flow(workflows, p, workflows("in"))) acc + 1 else acc)
    }.foldLeft(0L)((acc, sum) => acc + sum)
  }

  def flow(map: Map[String, List[Rule]], part: Part, rules: List[Rule]): Boolean =
    rules.head(part) match {
      case None => flow(map, part, rules.tail)
      case Some(Left(workflow)) => flow(map, part, map(workflow))
      case Some(Right(result)) => result
    }

  private def parse(lines: List[String]) = {
    val ind = lines.indexWhere(_.isEmpty())
    val (workflowStrings, partStrings) = (lines.take(ind), lines.drop(ind + 1))
    val workflows = workflowStrings.map { case s"$name{$rules}" => name -> rules.split(",").map(parseRule).toList }
    (workflows.toMap, partStrings.map(parsePart))
  }

  private def parseRule(string: String): Rule =
    part =>
      string match {
        case s"$prop<$num:$outcome" => Option.when(getter(prop, part) < num.toInt)(parseOutcome(outcome))
        case s"$prop>$num:$outcome" => Option.when(getter(prop, part) > num.toInt)(parseOutcome(outcome))
        case outcome => Some(parseOutcome(outcome))
      }

  private def parseOutcome(outcome: String) =
    outcome match {
      case "A" => Right(true)
      case "R" => Right(false)
      case other => Left(other)
    }

  private def parsePart(string: String): Part =
    string match { case (s"{x=$x,m=$m,a=$a,s=$s}") => Part(x.toInt, m.toInt, a.toInt, s.toInt) }
}
