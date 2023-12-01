package io.github.nikiforo

import scala.io.Source

package object aoc23 {

  def aocLines(file: String): List[String] = {
    val source = Source.fromFile(s"./src/main/resources/aoc/$file")
    val lines = source.getLines.toList
    source.close()
    lines
  }
}
