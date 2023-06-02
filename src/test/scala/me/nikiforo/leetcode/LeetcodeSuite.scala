package me.nikiforo.leetcode

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

trait LeetcodeSuite extends AnyFunSuite with ArrayOps {

  private val root = "./"

  protected def resource(file: String): List[String] = {
    val source = Source.fromFile(s"$root/src/main/resources/$file")
    val lines = source.getLines.toList
    source.close()
    lines
  }
}
