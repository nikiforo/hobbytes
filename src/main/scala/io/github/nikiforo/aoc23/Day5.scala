package io.github.nikiforo.aoc23

object Day5 {

  case class AlmRange(destStart: Long, sourceStart: Long, length: Long)

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("5"))}
         |task2: ${task2(aocLines("5"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) = {
    val (seeds, maps) = parse(lines)
    maps.foldLeft(seeds) { case (ids, ranges) =>
      ids.map { id =>
        val applyRangePF = Function.unlift(applyRange(id, _))
        ranges.collectFirst(applyRangePF).getOrElse(id)
      }
    }.min
  }

  def task2(lines: List[String]) = ""

  def parse(lines: List[String]) =
    (parseSeeds(lines.head), splitToMaps(lines.tail))

  def applyRange(id: Long, r: AlmRange): Option[Long] =
    if (r.sourceStart <= id && id < r.sourceStart + r.length) Some(id - r.sourceStart + r.destStart)
    else None

  private def splitToMaps(lines: List[String]): List[List[AlmRange]] = {
    val body = lines.drop(2)
    val emptyInd = body.indexWhere(_.isEmpty())
    if (emptyInd == -1) body.map(parseRange) :: Nil
    else {
      val (heads, tail) = body.splitAt(emptyInd)
      heads.map(parseRange) :: splitToMaps(tail)
    }
  }

  private def parseRange(line: String): AlmRange =
    line match { case s"$n1 $n2 $n3" => AlmRange(n1.toLong, n2.toLong, n3.toLong) }

  private def parseSeeds(line: String): List[Long] =
    line match { case s"seeds: $nums" => nums.split(" ").toList.map(_.toLong) }
}
