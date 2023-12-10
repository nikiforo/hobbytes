package io.github.nikiforo.aoc23

object Day10 {

  type Coord = (Int, Int)

  final class Scheme(lines: List[String]) {
    private val arr = lines.toArray.map(_.toArray)

    def indices = arr.indices.map(i => arr(i).indices.map((i, _)))

    def find(coord: Coord): Option[Char] = {
      val (i, j) = coord
      val isValid = i >= 0 && i < arr.length && j >= 0 && j < arr.head.length
      if (isValid) Some(arr(i)(j)) else None
    }

    def get(coord: Coord): Char = find(coord).get
  }

  object Directions {
    val goUp = (coord: Coord) => (coord._1 - 1, coord._2)
    val goDown = (coord: Coord) => (coord._1 + 1, coord._2)
    val goRight = (coord: Coord) => (coord._1, coord._2 + 1)
    val goLeft = (coord: Coord) => (coord._1, coord._2 - 1)

    def allDirections = List(goUp, goDown, goRight, goLeft)

    def getOuts(char: Char): List[Coord => Coord] = 
      char match {
        case ('|') => List(goUp, goDown)
        case ('-') => List(goLeft, goRight)
        case ('L') => List(goUp, goRight)
        case ('J') => List(goLeft, goUp)
        case ('7') => List(goLeft, goDown)
        case ('F') => List(goDown, goRight)
        case _ => List.empty
      }
  }
  
  private val cross = Set('|', 'J', 'L')

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("10"))}
         |task2: ${task2(aocLines("10"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) =
    buildRoute(new Scheme(lines)).map(_.size).max / 2

  def task2(lines: List[String]) = {
    val scheme = new Scheme(lines)
    val List(route) = buildRoute(scheme).map(_.toSet).distinct
    scheme.indices.map { inds =>
      inds.foldLeft((false, 0)) { case ((inside, qty), coord) =>
        val ch = scheme.get(coord)
        val intersect = cross(ch) || ch == 'S' && route(Directions.goUp(coord))
        if (route(coord) && intersect) (!inside, qty)
        else if(route(coord)) (inside, qty)
        else if (inside) (inside, qty + 1)
        else (inside, qty)
      }._2
    }.sum
  }

  private def buildRoute(scheme: Scheme) = {
    val start = scheme.indices.flatten.find(c => scheme.get(c) == 'S').get
    def go(coord: Coord, path: List[Coord]): Option[List[Coord]] =
      if (start == coord) Some(path)
      else
        for {
          c <- scheme.find(coord)
          outs = Directions.getOuts(c).map(_.apply(coord))
          next <- Option.when(outs.contains(path.head))(outs.find(_ != path.head)).flatten
          evalNext <- go(next, coord :: path)
        } yield evalNext

    Directions.allDirections.map(f => go(f(start), List(start))).flatten
  }
}
