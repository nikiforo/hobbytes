package io.github.nikiforo.aoc23

object Day4 {

  case class Card(num: Int, wins: List[Int], haves: List[Int]) {
    val winSet = wins.toSet
    val winCnt = haves.count(winSet)
  }

  def main(args: Array[String]): Unit = {
    val result =
      s"""task1: ${task1(aocLines("4"))}
         |task2: ${task2(aocLines("4"))}""".stripMargin
    println(result)
  }

  def task1(lines: List[String]) =
    lines.map { line =>
      val card = parse(line)
      if (card.winCnt == 0) 0 else Array.fill(card.winCnt - 1)(2).product
    }.sum

  def task2(lines: List[String]) =
    goTask2(lines.map(l => (1, parse(l))), 0)

  def goTask2(cards: List[(Int, Card)], acc: Int): Int =
    cards match {
      case Nil => acc
      case (amount, card) :: tail =>
        val (left, right) = tail.splitAt(card.winCnt)
        goTask2(left.map(pair => (pair._1 + amount, pair._2)) ::: right, acc + amount)
    }

  private val CardR = "Card +(\\d+): +(.+) \\| +(.+)".r

  def parse(line: String): Card =
    line match { case CardR(num, wins, haves) => Card(num.toInt, parseInts(wins), parseInts(haves)) }

  def parseInts(ints: String): List[Int] =
    ints.split(" +").map(_.toInt).toList
}
