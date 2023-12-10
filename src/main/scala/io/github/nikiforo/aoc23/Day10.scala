package io.github.nikiforo.aoc23

object Day10 {

  final class Scheme(lines: List[String]) {
    private val arr = lines.toArray.map(_.toArray)

    def indices = arr.indices.flatMap(i => arr(i).indices.map((i, _)))

    def find(coord: (Int, Int)) = {
      val (i, j) = coord
      val isInvalid = i < 0 || i >= arr.length || j < 0 || j >= arr.head.length
      if (isInvalid) None else Some(arr(i)(j))
    }
  }

  object Directions {
    private val goUp = (coord: (Int, Int)) => (coord._1 - 1, coord._2)
    private val goDown = (coord: (Int, Int)) => (coord._1 + 1, coord._2)
    private val goRight = (coord: (Int, Int)) => (coord._1, coord._2 + 1)
    private val goLeft = (coord: (Int, Int)) => (coord._1, coord._2 - 1)

    def allDirections = List(goUp, goDown, goRight, goLeft)

    def getOuts(char: Char): List[((Int, Int)) => (Int, Int)] = 
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

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("10"))}
         |task2: ${task2(aocLines("10"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) = {
    val scheme = new Scheme(lines)
    val start = scheme.indices.find(c => scheme.find(c) == Some('S')).get
    def go(prev: (Int, Int), coord: (Int, Int), distance: Int): Option[Int] =
      if (start == coord) Some(distance / 2)
      else
        for {
          c <- scheme.find(coord)
          outs = Directions.getOuts(c).map(_.apply(coord))
          next <- Option.when(outs.contains(prev))(outs.find(_ != prev)).flatten
          evalNext <- go(coord, next, distance + 1)
        } yield evalNext

    Directions.allDirections.map(f => go(start, f(start), distance = 1)).flatten.max
  }
  

  def task2(lines: List[String]) = ""
}