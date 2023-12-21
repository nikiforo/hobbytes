package io.github.nikiforo.aoc23

import io.github.nikiforo.aoc23.Direction._

object Day16 extends DayApp("16") {

  case class Light(i: Int, j: Int, direction: Direction)

  final class Scheme(lines: List[String]) {
    private val arr = lines.toArray.map(_.toArray)

    val (height, width) = (arr.length, arr(0).length)

    private val mirrorTopRight = Map[Direction, Direction](U -> R, R -> U, L -> D, D -> L)
    private val mirrorDownRight = Map[Direction, Direction](U -> L, R -> D, L -> U, D -> R)

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
        case L | R => List(direction)
        case U | D => List(L, R)
      }

    private def vertical(direction: Direction) =
      direction match {
        case U | D => List(direction)
        case L | R => List(U, D)
      }

    private def move(light: Light) =
      light.direction match {
        case U => light.copy(i = light.i - 1)
        case R => light.copy(j = light.j + 1)
        case L => light.copy(j = light.j - 1)
        case D => light.copy(i = light.i + 1)
      }
  }

  def task1(lines: List[String]) =
    dfs(new Scheme(lines), List(Light(0, 0, R)), Set.empty)

  def task2(lines: List[String]) = {
    val scheme = new Scheme(lines)
    def solve(i: Int, j: Int, d: Direction) = dfs(scheme, List(Light(i, j, d)), Set.empty)
    val is = (0 until scheme.height).flatMap(i => Array(solve(i, 0, R), solve(i, scheme.width - 1, L)))
    val js = (0 until scheme.width).flatMap(j => Array(solve(0, j, D), solve(scheme.height - 1, j, U)))
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
