package io.github.nikiforo.leetcode

object P739 {

  def dailyTemperatures(temperatures: Array[Int]): Array[Int] =
    temperatures.foldRight((List.empty[(Int, Int)], List.empty[Int])) { case (temp, (stack, result)) =>
      val nStack = insert(temp, 1, stack)
      (nStack, nStack.head._2 :: result)
    }._2.toArray

  def insert(temp: Int, acc: Int, stack: List[(Int, Int)]): List[(Int, Int)] =
    stack match {
      case Nil => List((temp, 0))
      case (otherTemp, steps) :: tail =>
        if (otherTemp > temp) (temp, acc) :: stack
        else insert(temp, acc + steps, tail)
    }
}
