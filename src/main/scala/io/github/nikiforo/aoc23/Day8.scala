package io.github.nikiforo.aoc23

import fs2.Stream

object Day8 extends DayApp("8") {

  private type Routes = Map[String, (String, String)]

  def task1(lines: List[String]) = {
    val map = lines.drop(2).map(parse).toMap
    solve(lines.head, Array("AAA"), map, _.forall(_ == "ZZZ"))
  }

  //❗️ It will take more than a thousand hours to compute the answer using my test case.
  def task2(lines: List[String]) = {
    val map = lines.drop(2).map(parse).toMap
    solve(lines.head, map.keys.filter(_.endsWith("A")).toArray, map, _.forall(_.endsWith("Z")))
  }

  private def solve(head: String, init: Array[String], map: Routes, finishF: Array[String] => Boolean): Long =
    zipLongPrintNum(go(instructions(head), map, init), 0).dropWhile(p => !finishF(p._1)).head._2

  private def zipLongPrintNum[T](list: LazyList[T], l: Long): LazyList[(T, Long)] = {
    if (l % 1_000_000 == 0) println(l)
    (list.head, l) #:: zipLongPrintNum(list.tail, l + 1)
  }

  private def go(instr: LazyList[Char], map: Routes, positions: Array[String]): LazyList[Array[String]] = {
    def next(t: (String, String)) = if (instr.head == 'L') t._1 else t._2
    positions #:: go(instr.tail, map, positions.map(p => next(map(p))))
  }

  private def instructions(line: String): LazyList[Char] =
    LazyList.from(line) #::: instructions(line)

  private def parse(line: String) =
    line match { case s"$from = ($left, $right)" => (from, (left, right)) }
}
