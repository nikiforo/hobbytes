package io.github.nikiforo.aoc23

object Day5 {

  case class AlmRange(destStart: Long, sourceStart: Long, length: Long)

  case class SeedRange(start: Long, end: Long)

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("5"))}
         |task2: ${task2(aocLines("5"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) = {
    val seeds = parseSeeds(lines.head).map(l => SeedRange(l, l))
    val maps = splitToMaps(lines.tail)
    applyToSeed(seeds, maps)
  }

  def task2(lines: List[String]) = {
    val seeds = parseSeeds(lines.head).grouped(2).collect { case List(start, len) => SeedRange(start, start + len - 1) }
    val maps = splitToMaps(lines.tail)
    applyToSeed(seeds.toList, maps)
  }

  def applyToSeed(init: List[SeedRange], maps: List[List[AlmRange]]): Long =
    maps.foldLeft(init) { case (seeds, ranges) => goApply(List.empty, seeds, ranges) }.map(_.start).min

  def goApply(done: List[SeedRange], seeds: List[SeedRange], ranges: List[AlmRange]): List[SeedRange] =
    seeds match {
      case Nil => done
      case seed :: tail =>
        val pf = Function.unlift(intersectRanges(seed, _))
        ranges.collectFirst(pf) match {
          case None => goApply(seed :: done, tail, ranges)
          case Some((intersect, others)) => goApply(intersect :: done, others ::: tail, ranges)
        }
    }

  def intersectRanges(seed: SeedRange, r: AlmRange): Option[(SeedRange, List[SeedRange])] = {
    val rRight = r.sourceStart + r.length - 1
    if (seed.end < r.sourceStart || rRight < seed.start) None
    else {
      val intersectLeft = math.max(seed.start, r.sourceStart)
      val intersectRight = math.min(seed.end, rRight)
      val left = Option.when(seed.start < intersectLeft)(SeedRange(seed.start, intersectLeft - 1))
      val right = Option.when(intersectRight < seed.end)(SeedRange(intersectRight + 1, seed.end))
      val offset = r.destStart - r.sourceStart
      Some(SeedRange(intersectLeft + offset, intersectRight + offset), List(left, right).flatten)
    }
  }

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
