package io.github.nikiforo.leetcode

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import org.scalactic.Prettifier
import org.scalactic.source

trait LeetcodeSuite extends AnyFunSuite with ArrayOps {

  private val root = "./"

  protected def resource(file: String): List[String] = {
    val source = Source.fromFile(s"$root/src/main/resources/$file")
    val lines = source.getLines.toList
    source.close()
    lines
  }

  protected def assertResultTime[T](expected: T)(actual: => T)(implicit prettifier: Prettifier, pos: source.Position) = {
    val start = System.currentTimeMillis()
    val act = actual
    val dur = System.currentTimeMillis() - start
    if (dur >= 100) println(s"\n\nTOOK: ${dur}ms\n")
    assertResult(expected)(act)
  }
    
}
