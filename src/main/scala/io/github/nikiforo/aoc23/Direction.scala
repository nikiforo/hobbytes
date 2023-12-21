package io.github.nikiforo.aoc23

sealed trait Direction { def string: String }

object Direction {
  case object U extends Direction { def string = "U" }
  case object R extends Direction { def string = "R" }
  case object D extends Direction { def string = "D" }
  case object L extends Direction { def string = "L" }

  val all = List(U, R, L, D)

  val fromString: Map[String, Direction] = all.map(d => d.string -> d).toMap
}
