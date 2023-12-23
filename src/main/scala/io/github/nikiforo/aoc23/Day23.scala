package io.github.nikiforo.aoc23

import io.github.nikiforo.aoc23.Direction._

object Day23 extends DayApp("23") {

  def task1(lines: List[String]): Long = {
    val arr = lines.toArray.map(_.toArray)
    val start = Coord(0, arr(0).indices.find(arr(0)(_) == '.').get)
    go(arr, nextDirs1, List((start, Set.empty)), 0)
  }

  def go(arr: Array[Array[Char]], nDirs: Char => List[Direction], visit: List[(Coord, Set[Coord])], max: Long): Long =
    visit match {
      case Nil => max
      case (coord, visited) :: tail =>
        if (visited(coord)) go(arr, nDirs, tail, max)
        else if (!arr.inBorder(coord)) go(arr, nDirs, tail, max)
        else if (coord.i == arr.length - 1 && arr.get(coord) == '.') go(arr, nDirs, tail, math.max(max, visited.size))
        else {
          val nVisited = visited + coord
          go(arr, nDirs, nDirs(arr.get(coord)).map(d => (coord.move(d), nVisited)) ::: tail, max)
        }
    }

  private def nextDirs1(c: Char) =
    c match {
      case '#' => List.empty
      case '>' => List(R)
      case '<' => List(L)
      case 'v' => List(D)
      case '^' => List(U)
      case _ => all
    }

  private def nextDirs2(c: Char) =
    c match {
      case '#' => List.empty
      case _ => all
    }

  def task2(lines: List[String]): Long = timeouted {
    val arr = lines.toArray.map(_.toArray)
    val start = Coord(0, arr(0).indices.find(arr(0)(_) == '.').get)
    go(arr, nextDirs2, List((start, Set.empty)), 0)
  }
}
