package io.github.nikiforo.aoc23

object Day2 {

  case class Game(num: Int, hands: List[Hand])
  case class Hand(cubes: List[CubesColor])
  case class CubesColor(qty: Int, color: Color)

  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color

  private val allCubes: Map[Color, Int] = Map(Red -> 12, Green -> 13, Blue -> 14)

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("2"))}
         |task2: ${task2(aocLines("2"))}""".stripMargin
    println(result)
  }

  def task1(games: List[String]) =
    games.map(GameParser.parse).filter(_.hands.forall(_.cubes.forall(checkCubes))).map(_.num).sum

  def task2(games: List[String]) =
    games.map(GameParser.parse).map(computePower).sum

  private def computePower(game: Game): Int = {
    val colors =
      for {
        hand <- game.hands
        cubeColor <- hand.cubes
      } yield
        cubeColor.color match {
          case Red => (cubeColor.qty, 0, 0)
          case Green => (0, cubeColor.qty, 0)
          case Blue => (0, 0, cubeColor.qty)
        }

    val (reds, greens, blues) = colors.unzip3
    reds.max * greens.max * blues.max
  }

  private def checkCubes(cubes: CubesColor) =
    cubes.qty <= allCubes(cubes.color)

  object GameParser {

    private val GameRegexp = "Game (\\d+): (.*)".r

    private val CubesRegexp = "(\\d+) (.*)".r

    def parse(line: String) =
      line match {
        case GameRegexp(num, hands) => Game(num.toInt, hands.split("; ").toList.map(parseHand))
      }

    private def parseHand(string: String): Hand = {
      val cubes = string.split(", ").toList.map {
        case CubesRegexp(num, color) => CubesColor(num.toInt, parseColor(color))
      }
      Hand(cubes)
    }

    private def parseColor(color: String): Color =
      color match {
        case "red" => Red
        case "green" => Green
        case "blue" => Blue
      }
  }
}
