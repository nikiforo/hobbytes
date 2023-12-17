package io.github.nikiforo

import scala.io.Source

package object aoc23 {

  def aocLines(file: String): List[String] = {
    val source = Source.fromFile(s"./src/main/resources/aoc/$file")
    val lines = source.getLines.toList
    source.close()
    lines
  }

  sealed trait Direction
  case object Up extends Direction
  case object Right extends Direction
  case object Left extends Direction
  case object Down extends Direction

  val allDirs = List(Up, Right, Left, Down)

  val dirOpposite: Map[Direction, Direction] = Map(Up -> Down, Right -> Left, Left -> Right, Down -> Up)

  val dir3: Map[Direction, List[Direction]] = allDirs.map(d => d -> allDirs.filter(_ != dirOpposite(d))).toMap

  case class Coord(i: Int, j: Int)

  def move(coord: Coord, direction: Direction): Coord =
    direction match {
      case Up => coord.copy(i = coord.i - 1)
      case Right => coord.copy(j = coord.j + 1)
      case Left => coord.copy(j = coord.j - 1)
      case Down => coord.copy(i = coord.i + 1)
    }

  def inBorder[T](c: Coord, arr: Array[Array[T]]): Boolean =
    c.i >= 0 && c.i < arr.length && c.j >= 0 && c.j < arr(0).length

  def get[T](c: Coord, arr: Array[Array[T]]): T =
    arr(c.i)(c.j)

  def lastCoord[T](arr: Array[Array[T]]): Coord =
    Coord(arr.length - 1, arr(0).length - 1)
}
