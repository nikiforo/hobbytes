package io.github.nikiforo.aoc23

object Day4 extends DayApp("4") {

  case class Card(num: Int, wins: List[Int], haves: List[Int])

  def task1(lines: List[String]) =
    lines.map { line =>
      val cnt = winCnt(parse(line))
      if (cnt == 0) 0 else Array.fill(cnt - 1)(2).product
    }.sum

  def task2(lines: List[String]) =
    goTask2(lines.map(l => (1, parse(l))), 0)

  private def winCnt(card: Card): Int = {
    val winSet = card.wins.toSet
    card.haves.count(winSet)
  }

  private def goTask2(cards: List[(Int, Card)], acc: Int): Int =
    cards match {
      case Nil => acc
      case (amount, card) :: tail =>
        val (left, right) = tail.splitAt(winCnt(card))
        goTask2(left.map(pair => (pair._1 + amount, pair._2)) ::: right, acc + amount)
    }

  private val CardR = "Card +(\\d+): +(.+) \\| +(.+)".r

  private def parse(line: String): Card =
    line match { case CardR(num, wins, haves) => Card(num.toInt, parseInts(wins), parseInts(haves)) }

  private def parseInts(ints: String): List[Int] =
    ints.split(" +").map(_.toInt).toList
}
