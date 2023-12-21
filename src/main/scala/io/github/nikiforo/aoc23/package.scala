package io.github.nikiforo

import io.github.nikiforo.aoc23.Direction._

package object aoc23 {

  case class Coord(i: Int, j: Int)

  implicit class CoordOps(val coord: Coord) extends AnyVal {

    def move(direction: Direction): Coord =
      direction match {
        case U => coord.copy(i = coord.i - 1)
        case R => coord.copy(j = coord.j + 1)
        case L => coord.copy(j = coord.j - 1)
        case D => coord.copy(i = coord.i + 1)
      }
  }

  implicit class ArrayOps[T](val arr: Array[Array[T]]) extends AnyVal {

    def coords = arr.indices.flatMap(i => arr(i).indices.map(Coord(i, _)))

    def inBorder(c: Coord): Boolean =
      c.i >= 0 && c.i < arr.length && c.j >= 0 && c.j < arr(0).length

    def get(c: Coord): T =
      arr(c.i)(c.j)

    def lastCoord: Coord =
      Coord(arr.length - 1, arr(0).length - 1)
  }

  val initCoord = Coord(0, 0)

  val dirOpposite: Map[Direction, Direction] = Map(U -> D, R -> L, L -> R, D -> U)
  val dirRight: Map[Direction, Direction] = Map(U -> R, R -> D, D -> L, L -> U)
  val dir3: Map[Direction, List[Direction]] = all.map(d => d -> all.filter(_ != dirOpposite(d))).toMap
}
