package io.github.nikiforo.aoc23

object Day13 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("13"))}
         |task2: ${task2(aocLines("13"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) =
    parse(lines).map(scheme => getMirror(scheme.map(_.toList))).sum

  def task2(lines: List[String]) =
    parse(lines).map { scheme =>
      val twoDim = scheme.map(_.toList)
      val inds = twoDim.indices.flatMap(i => twoDim(i).indices.map((i, _)))
      val (orig1, orig2) = (mirrors(twoDim), mirrors(twoDim.transpose))
      val smudgedSchemes = inds.view.map { case (i, j) => twoDim.updated(i, invert(twoDim(i), j)) }
      smudgedSchemes.flatMap { smugled =>
        val horizontal = (mirrors(smugled.transpose) -- orig2).headOption.map(_ * 100)
        (mirrors(smugled) -- orig1).headOption.orElse(horizontal)
      }.head
    }.sum

  private def parse(lines: List[String]): List[List[String]] = {
    val ind = lines.indexWhere(_.isEmpty())
    if (ind == -1) List(lines)
    else {
      val (left, right) = lines.splitAt(ind)
      left :: parse(right.tail)
    }
  }

  private def getMirror(twoDim: List[List[Char]]): Int =
    mirrors(twoDim).headOption.getOrElse(mirrors(twoDim.transpose).head * 100)

  private def mirrors(twoDim: List[List[Char]]): Set[Int] =
    twoDim.map(row => rowMirrors(0, row, List.empty, Set.empty)).reduce(_ & _)

  private def invert(row: List[Char], i: Int): List[Char] =
    row.updated(i, if (row(i) == '#') '.' else '#')

  private def rowMirrors(ind: Int, row: List[Char], acc: List[Char], set: Set[Int]): Set[Int] =
    row match {
      case Nil => set - 0
      case h :: tail =>
        val nSet = if (acc.startsWith(row) || row.startsWith(acc)) set + ind else set
        rowMirrors(ind + 1, tail, h :: acc, nSet)
    }
}
