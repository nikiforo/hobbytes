package io.github.nikiforo.aoc23

import scala.collection.mutable.Stack
import scala.collection.mutable.Set

object Day3 {
  
  final class Scheme(lines: List[String]) {
    private val scheme = lines.toArray.map(_.toArray)

    def indices = scheme.indices.flatMap(i => scheme(i).indices.map(j => (i, j)))

    def isDigit(c: (Int, Int)) = char(c).isDigit

    def isDot(c: (Int, Int)) = char(c) == '.'

    def isGear(c: (Int, Int)) = char(c) == '*'

    private def char(c: (Int, Int)) = scheme(c._1)(c._2)

    def expand(c: (Int, Int)) = {
      def nonDigit(end: Int, step: Int) = c._2.to(end, step).find(j => !char((c._1, j)).isDigit).fold(end)(_ - step)
      (nonDigit(0, -1), nonDigit(scheme(0).length - 1, 1))
    }

    def validNeighbours8(coords: (Int, Int)) =
      neighbours8(coords).filter(inBorders)

    def string(i: Int, l: Int, r: Int) = new String(scheme(i), l, r - l + 1)

    private def neighbours8(coords: (Int, Int)) = {
      val (i, j) = coords
      List(
        (i - 1, j - 1), (i - 1, j), (i - 1, j + 1),
        (i, j - 1), (i, j + 1),
        (i + 1, j - 1), (i + 1, j), (i + 1, j + 1),
      )
    }

    private def inBorders(c: (Int, Int)) =
      c._1 >= 0 && c._1 < scheme.length && c._2 >= 0 && c._2 < scheme(0).length

  }

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("3"))}
         |task2: ${task2(aocLines("3"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) = {
    val sc = new Scheme(lines)
    val stack = Stack.from(sc.indices.filter(c => !sc.isDigit(c) && !sc.isDot(c)))
    val visited = Set.empty[(Int, Int)]
    val nums = List.newBuilder[Int]
    def toVisit(c: (Int, Int)) = sc.validNeighbours8(c).filter(!sc.isDot(_)).filter(!visited(_))
    while (stack.nonEmpty) {
      val coord = stack.pop()
      visited.add(coord)
      val (digits, others) = toVisit(coord).partition(sc.isDigit(_))
      val ranges = digits.map { case c @ (i, _) => (i, sc.expand(c)) }.distinct
      nums.addAll(ranges.map { case (i, (l, r)) => sc.string(i, l, r).toInt })
      val rangeCoords = ranges.flatMap { case (i, (l, r)) => l.until(r).map((i, _)) }
      visited.addAll(rangeCoords)
      stack.addAll(rangeCoords.flatMap(toVisit).distinct ++ others)
    }
    nums.result().sum
  }

  def task2(lines: List[String]) = {
    val sc = new Scheme(lines)
    val gears = sc.indices.filter(sc.isGear(_))
    gears.map { g =>
      val digits = sc.validNeighbours8(g).filter(sc.isDigit)
      val nums = digits.map { case c @ (i, _) => (i, sc.expand(c)) }.distinct
      if (nums.length != 2) 0
      else nums.map { case (i, (l, r)) => sc.string(i, l, r).toInt }.product
    }.sum
  }
}
