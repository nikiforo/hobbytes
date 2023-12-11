package io.github.nikiforo.aoc23

object Day11 {

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("11"))}
         |task2: ${task2(aocLines("11"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) = 
    solve(lines, k = 2)

  def task2(lines: List[String]) =
    solve(lines, k = 1000000)

  def solve(lines: List[String], k: Long) = {
    val expanded = expand(lines.toArray.map(_.toArray))
    val indices = expanded.indices.flatMap(i => expanded(i).indices.map((i, _)))
    val galaxies = indices.filter(c => expanded(c._1)(c._2) == '#').toArray
    galaxies.combinations(2).collect { case Array((i1, j1), (i2, j2)) =>
      val is = iter(i1, i2).map(i => if(expanded(i)(j1) == '*') k else 1)
      val js = iter(j1, j2).map(j => if(expanded(i1)(j) == '*') k else 1)
      is.sum + js.sum - 2
    }.sum
  }

  private def iter(i1: Int, i2: Int) =
    if (i1 < i2) i1.to(i2) else i2.to(i1)

  private def expand(galaxies: Array[Array[Char]]) =
    galaxies.map(expandRow).transpose.map(expandRow).transpose

  private def expandRow(row: Array[Char]): Array[Char] =
    if (row.exists(_ == '#')) row else Array.fill(row.length)('*')
}
