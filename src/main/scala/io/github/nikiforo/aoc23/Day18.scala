package io.github.nikiforo.aoc23

import io.github.nikiforo.aoc23.Coord
import io.github.nikiforo.aoc23.Direction
import scala.collection.mutable
import io.github.nikiforo.aoc23.initCoord
import io.github.nikiforo.aoc23.dirRight

object Day18 extends DayApp("18") {

  case class DigPlanEntry(dir: Direction, steps: Int, colorHex: String)

  def task1(lines: List[String]): Long = {
    val directions = lines.map(parse).flatMap(e => List.fill(e.steps)(e.dir))
    
    val init = (initCoord, Set(initCoord), List.empty[Coord])
    val (_, path, inners) = directions.foldLeft(init) { case ((coord, set, inner), dir) =>
      val nCoord = coord.move(dir)
      (nCoord, set + nCoord, nCoord.move(dirRight(dir)) :: inner)
    }

    fill(inners, path)
  }

  def task2(lines: List[String]): Long = unsolved

  private def fill(visit: List[Coord], seen: Set[Coord]): Int =
    visit match {
      case Nil => seen.size
      case h :: tail =>
        if (seen.contains(h)) fill(tail, seen)
        else fill(Direction.all.map(h.move) ::: tail, seen + h)
    }

  private def parse(line: String): DigPlanEntry =
    line match { case s"$d $s (#$clr)" => DigPlanEntry(Direction.fromString(d), s.toInt, clr) }
}
