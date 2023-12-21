package io.github.nikiforo.aoc23

import org.scalatest.funsuite.AnyFunSuite

class Day15Suite extends AnyFunSuite {
  val initSeq = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

  test("task2") {
    assert(Day15.task2(List(initSeq)) == 145)
  }
}
