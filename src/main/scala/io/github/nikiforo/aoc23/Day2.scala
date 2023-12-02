package io.github.nikiforo.aoc23

object Day2 {

  private case class Game(num: Int, hands: List[Hand])
  private case class Hand(cubes: List[CubesColor])
  private case class CubesColor(qty: Int, color: String)

  private val allCubes: Map[String, Int] = Map("red" -> 12, "green" -> 13, "blue" -> 14)

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
    
  private def checkCubes(cubes: CubesColor) =
    cubes.qty <= allCubes(cubes.color)

  private def computePower(game: Game): Int = {
    val colors =
      for {
        hand <- game.hands
        cubeColor <- hand.cubes
      } yield
        cubeColor.color match {
          case "red" => (cubeColor.qty, 0, 0)
          case "green" => (0, cubeColor.qty, 0)
          case "blue" => (0, 0, cubeColor.qty)
        }

    val (reds, greens, blues) = colors.unzip3
    reds.max * greens.max * blues.max
  }

  private object GameParser {

    private val GameR = "Game (\\d+): (.*)".r

    private val CubesR = "(\\d+) (.*)".r

    def parse(line: String) =
      line match {
        case GameR(num, hands) => Game(num.toInt, hands.split("; ").toList.map(parseHand))
      }

    private def parseHand(string: String): Hand =
      Hand(string.split(", ").toList.map { case CubesR(num, color) => CubesColor(num.toInt, color) })
  }
}
