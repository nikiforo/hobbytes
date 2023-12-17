package io.github.nikiforo.aoc23

import scala.collection.immutable.TreeMap
import io.github.nikiforo.aoc23.Coord
import io.github.nikiforo.aoc23.Direction

object Day17 extends DayApp("17") {

  case class Path(coord: Coord, dir: Direction, foward: Int)

  implicit class TreeMapQueue(val treeMap: TreeMap[Int, List[Path]]) extends AnyVal {

    def dequeue: ((Int, Path), TreeMap[Int, List[Path]]) = {
      val (heatLost, paths) = treeMap.head
      val nMap = if (paths.tail.isEmpty) treeMap.removed(heatLost) else treeMap.updated(heatLost, paths.tail)
      ((heatLost, paths.head), nMap)
    }

    def enqueue(entry: (Int, Path)): TreeMap[Int, List[Path]] =
      treeMap.updated(entry._1, entry._2 :: treeMap.getOrElse(entry._1, Nil))
  }

  def task1(lines: List[String]) =
    dij(lines.map(_.toArray).toArray, TreeMap(0 -> List(Path(Coord(0, 0), Right, 0))), Set.empty, nextDirs1)

  def task2(lines: List[String]) =
    dij(lines.map(_.toArray).toArray, TreeMap(0 -> List(Path(Coord(0, 0), Right, 0))), Set.empty, nextDirs2)

  private def dij(
    arr: Array[Array[Char]],
    treeMap: TreeMap[Int, List[Path]],
    seen: Set[Path],
    nextDirF: (Direction, Int) => List[Direction],
  ): Int = {
    val ((heatLost, path), nVisit) = treeMap.dequeue
    if (path.coord == lastCoord(arr)) heatLost
    else {
      val nextPaths = nextDirF(path.dir, path.foward).map(movePath(path, _))
      val next = nextPaths.filter(p => inBorder(p.coord, arr) && !seen.contains(path))
      val toVisit = next.foldLeft(nVisit)((v, p) => v.enqueue(heatLost + get(p.coord, arr) - '0', p))
      dij(arr, toVisit, seen + path, nextDirF)
    }
  }

  private def movePath(path: Path, d: Direction) =
    Path(move(path.coord, d), d, if (d == path.dir) path.foward + 1 else 1)

  private def nextDirs1(dir: Direction, forward: Int): List[Direction] =
    if (forward < 3) dir3(dir) else dir3(dir).filter(_ != dir)

  private def nextDirs2(dir: Direction, forward: Int): List[Direction] =
    if (forward < 4) List(dir)
    else if (forward < 10) dir3(dir)
    else dir3(dir).filter(_ != dir)
}
