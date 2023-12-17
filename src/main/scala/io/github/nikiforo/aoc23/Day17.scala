package io.github.nikiforo.aoc23

import scala.collection.immutable.TreeMap
import io.github.nikiforo.aoc23.Coord
import io.github.nikiforo.aoc23.Direction
import scala.collection.immutable.TreeSet

object Day17 extends DayApp("17") {

  case class Path(coord: Coord, direction: Direction, stepsFoward: Int)

  private implicit val ord: Ordering[(Int, Path)] =
    Ordering.by { case (heatLost, p) => (heatLost, p.coord.i, p.coord.j, p.direction.string, p.stepsFoward) }

  def task1(lines: List[String]) =
    dijkstra(lines.map(_.toArray).toArray, TreeSet((0, Path(Coord(0, 0), Right, 0))), Set.empty, nextDirs1)

  def task2(lines: List[String]) =
    dijkstra(lines.map(_.toArray).toArray, TreeSet((0, Path(Coord(0, 0), Right, 0))), Set.empty, nextDirs2)

  private def dijkstra(
    scheme: Array[Array[Char]],
    visit: TreeSet[(Int, Path)],
    seen: Set[Path],
    nextDirectionF: (Direction, Int) => List[Direction],
  ): Int = {
    val (heatLost, path) = visit.head
    if (path.coord == lastCoord(scheme)) heatLost
    else {
      val nextPaths = nextDirectionF(path.direction, path.stepsFoward).map(movePath(path, _))
      val next = nextPaths.filter(p => inBorder(p.coord, scheme) && !seen.contains(path))
      val toVisit = next.foldLeft(visit.tail)((v, p) => v + ((heatLost + get(p.coord, scheme) - '0', p)))
      dijkstra(scheme, toVisit, seen + path, nextDirectionF)
    }
  }

  private def movePath(path: Path, d: Direction) =
    Path(move(path.coord, d), d, if (d == path.direction) path.stepsFoward + 1 else 1)

  private def nextDirs1(dir: Direction, forward: Int): List[Direction] =
    if (forward < 3) dir3(dir) else dir3(dir).filter(_ != dir)

  private def nextDirs2(dir: Direction, forward: Int): List[Direction] =
    if (forward < 4) List(dir)
    else if (forward < 10) dir3(dir)
    else dir3(dir).filter(_ != dir)
}
