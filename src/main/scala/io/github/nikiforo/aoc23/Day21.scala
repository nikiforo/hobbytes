package io.github.nikiforo.aoc23

object Day21 extends DayApp("21") {

  def task1(lines: List[String]): Long =
    run1(lines, 64)

  def run1(lines: List[String], steps: Int): Long = {
    val arr = lines.toArray.map(_.toArray)
    (1 to steps).foldLeft(Set(arr.coords.find(c => arr.get(c) == 'S').get)) { (set, _) =>
      set.flatMap(coord => Direction.all.map(coord.move).filter(arr.get(_) != '#'))
    }.size
  }

  def task2(lines: List[String]): Long = unsolved
}
