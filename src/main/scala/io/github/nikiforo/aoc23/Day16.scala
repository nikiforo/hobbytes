package io.github.nikiforo.aoc23

object Day16 {

  sealed trait Direction
  case object Up extends Direction
  case object Right extends Direction
  case object Left extends Direction
  case object Down extends Direction

  case class Light(i: Int, j: Int, direction: Direction)

  final class Scheme(lines: List[String]) {
    private val arr = lines.toArray.map(_.toArray)

    val (height, width) = (arr.length, arr(0).length)

    private val mirrorTopRight = Map[Direction, Direction](Up -> Right, Right -> Up, Left -> Down, Down -> Left)
    private val mirrorDownRight = Map[Direction, Direction](Up -> Left, Right -> Down, Left -> Up, Down -> Right)

    def next(light: Light): Option[List[Light]] =
      if (light.i < 0 || light.i >= arr.length || light.j < 0 || light.j >= arr(light.i).length) None
      else {
        val directions =
          arr(light.i)(light.j) match {
            case '/' => List(mirrorTopRight(light.direction))
            case '\\' => List(mirrorDownRight(light.direction))
            case '-' => horizontal(light.direction)
            case '|' => vertical(light.direction)
            case _ => List(light.direction)
          }
        Some(directions.map(d => move(light.copy(direction = d))))
      }

    private def horizontal(direction: Direction) =
      direction match {
        case Left | Right => List(direction)
        case Up | Down => List(Left, Right)
      }

    private def vertical(direction: Direction) =
      direction match {
        case Up | Down => List(direction)
        case Left | Right => List(Up, Down)
      }

    private def move(light: Light) =
      light.direction match {
        case Up => Light(light.i - 1, light.j, Up)
        case Right => Light(light.i, light.j + 1, Right)
        case Left => Light(light.i, light.j - 1, Left)
        case Down => Light(light.i + 1, light.j, Down)
      }
  }

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("16"))}
         |task2: ${task2(aocLines("16"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) =
    dfs(new Scheme(lines), List(Light(0, 0, Right)), Set.empty)

  def task2(lines: List[String]) = {
    val scheme = new Scheme(lines)
    def solve(i: Int, j: Int, d: Direction) = dfs(scheme, List(Light(i, j, d)), Set.empty)
    val is = (0 until scheme.height).flatMap(i => Array(solve(i, 0, Right), solve(i, scheme.width - 1, Left)))
    val js = (0 until scheme.width).flatMap(j => Array(solve(0, j, Down), solve(scheme.height - 1, j, Up)))
    math.max(is.max, js.max)
  }

  private def dfs(scheme: Scheme, visit: List[Light], visited: Set[Light]): Int =
    visit match {
      case Nil => visited.map(v => (v.i, v.j)).size
      case h :: tail =>
        if (visited(h)) dfs(scheme, tail, visited)
        else scheme.next(h) match {
          case None => dfs(scheme, tail, visited)
          case Some(lights) => dfs(scheme, lights ::: tail, visited + h)
        }
    }
}
