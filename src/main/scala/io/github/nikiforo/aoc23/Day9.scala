package io.github.nikiforo.aoc23

object Day9 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("9"))}
         |task2: ${task2(aocLines("9"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) =
    parse(lines).map(go(_, List.empty)).sum

  def task2(lines: List[String]) =
    parse(lines).map(l => go(l.reverse, List.empty)).sum

  def go(nums: List[Long], prevs: List[Long]): Long =
    if (nums.forall(_ == 0)) prevs.foldLeft(0L)((bottom, left) => bottom + left)
    else go(nums.zip(nums.tail).map(p => p._2 - p._1), nums.last :: prevs)

  private def parse(lines: List[String]) =
    lines.map(_.split(" ").map(_.toLong).toList)
}
