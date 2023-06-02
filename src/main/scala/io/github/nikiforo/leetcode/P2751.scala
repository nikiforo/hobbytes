package io.github.nikiforo.leetcode

object P2751 {

  case class Robot(pos: Int, health: Int, isRight: Boolean, index: Int)

  def survivedRobotsHealths(positions: Array[Int], healths: Array[Int], directions: String): List[Int] = {
    val raw = positions.zip(healths).zip(directions).zipWithIndex
    val robots = raw.map { case (((pos, h), dir), ind) => Robot(pos, h, dir == 'R', ind) }
    go(robots.sortBy(_.pos).toList, List.empty, List.empty).sortBy(_.index).map(_.health)
  }

  def go(robots: List[Robot], goRight: List[Robot], goLeft: List[Robot]): List[Robot] =
    robots match {
      case Nil => goLeft ++ goRight
      case robot :: tail =>
        if (robot.isRight) go(tail, robot :: goRight, goLeft)
        else {
          val (nRight, nLeft) = collide(goRight, robot)
          go(tail, nRight, List.from(nLeft) ::: goLeft)
        }
    }

  def collide(goRight: List[Robot], robot: Robot): (List[Robot], Option[Robot]) =
    goRight match {
      case Nil => (Nil, Some(robot))
      case other :: tail =>
        if (other.health < robot.health) collide(tail, robot.copy(health = robot.health - 1))
        else if (other.health == robot.health) (tail, None)
        else (other.copy(health = other.health - 1) :: tail, None)
    }
}
