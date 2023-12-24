package io.github.nikiforo.aoc23

object Day24 extends DayApp("24") {

  case class Hail(x: Long, y: Long, z: Long, vx: Long, vy: Long, vz: Long)

  def task1(lines: List[String]): Long =
    intersections(lines.map(parse).toArray, 200000000000000L, 400000000000000L).size

  def intersections(arr: Array[Hail], from: Long, to: Long) =
    for {
      i <- arr.indices
      j <- i + 1 until arr.length
      (a, b) = (arr(i), arr(j))
      coordinates @ (x, y) <- intersect2d(a, b)
      if from < x && x < to
      if from < y && y < to
      if timeToReach(a, coordinates) > 0 && timeToReach(b, coordinates) > 0
    } yield (a, b)

  def timeToReach(from: Hail, to: (Double, Double)) =
    if (from.vx == 0) (to._2 - from.y) / from.vy
    else (to._1 - from.x) / from.vx

  def task2(lines: List[String]): Long = unsolved

  def parse(string: String) =
    string match { case s"$x, $y, $z @ $vx, $vy, $vz" =>
      Hail(x.toLong, y.toLong, z.toLong, vx.toLong, vy.toLong, vz.toLong)
    }

  def intersect2d(h1: Hail, h2: Hail): Option[(Double, Double)] = {
    val (x1, x2, x3, x4) = (h1.x, h1.x + h1.vx, h2.x, h2.x + h2.vx)
    val (y1, y2, y3, y4) = (h1.y, h1.y + h1.vy, h2.y, h2.y + h2.vy)
    val xNumerator = (x1*y2 - y1*x2)*(x3 - x4) - (x1 - x2)*(x3*y4 - y3*x4)
    val xDenomenator = (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
    val yNumerator = (x1 * y2 - y1*x2)*(y3 - y4) - (y1 - y2)*(x3 * y4 - y3 * x4)
    val yDenomenator = (x1 - x2)*(y3 - y4) - (y1 - y2) * (x3 - x4)
    val coordinates = (xNumerator.toDouble / xDenomenator, yNumerator.toDouble / yDenomenator)
    Option.when(xDenomenator != 0 && yDenomenator != 0)(coordinates)
  }
}
