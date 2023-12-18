package io.github.nikiforo.aoc23

object Day7 extends DayApp("7") {

  case class Entry(hand: String, bid: Long)

  private val cardCosts1: Map[Char, Int] =
    List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').reverse.zipWithIndex.toMap

  private val cardCosts2: Map[Char, Int] =
    List('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse.zipWithIndex.toMap

  def task1(lines: List[String]) =
    solvePuzzle(lines, ord1)

  def task2(lines: List[String]) =
    solvePuzzle(lines, ord2)

  private def solvePuzzle(lines: List[String], order: Entry => (Int, Long)) =
    lines.map(parse).sortBy(order).zipWithIndex.map { case (e, rank) => e.bid * (rank + 1) }.sum

  private def ord1(entry: Entry) = {
    val aggregated = entry.hand.groupBy(identity).toList.map(_._2.size).sorted.reverse
    (getHandType(aggregated), singleCardCost(entry.hand, cardCosts1))
  }

  private def ord2(entry: Entry) = {
    val aggregated =
      entry.hand.filter(_ != 'J').groupBy(identity).toList.map(_._2.size).sorted.reverse match {
        case Nil => List(5)
        case h :: tail => (h + entry.hand.count(_ == 'J')) :: tail
      }
    (getHandType(aggregated), singleCardCost(entry.hand, cardCosts2))
  }

  private def getHandType(aggregated: List[Int]) =
    aggregated match {
      case List(5) => 0
      case List(4, 1) => -1
      case List(3, 2) => -2
      case List(3, 1, 1) => -3
      case List(2, 2, 1) => -4
      case List(2, 1, 1, 1) => -5
      case _ => -6
    }

  private def singleCardCost(hand: String, cardCosts: Map[Char, Int]): Long =
    ("1" + hand.map(cardCosts).map(c => f"$c%02d").mkString).toLong

  private def parse(line: String): Entry =
    line match { case s"$hand $bid" => Entry(hand, bid.toLong) }
}
