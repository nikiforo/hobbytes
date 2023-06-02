package io.github.nikiforo.leetcode

import io.circe.parser.parse
import io.circe.syntax._
import io.github.nikiforo.leetcode.ArrayOps._

trait ArrayOps {

  implicit def argumentsOps(string: String) =
    new ArrayStringOps(string)

  implicit def ararOps[T](arr: Array[Array[T]]) =
    new ArarOps(arr)
}

object ArrayOps {

  final class ArrayStringOps(val string: String) extends AnyVal {

    def ararchar: Array[Array[Char]] = parse(string).flatMap(_.as[Array[Array[Char]]]).toTry.get

    def ararint: Array[Array[Int]] = parse(string).flatMap(_.as[Array[Array[Int]]]).toTry.get

    def lilint: List[List[Int]] = ararint.map(_.toList).toList

    def arstring: Array[String] = parse(string).flatMap(_.as[Array[String]]).toTry.get

    def listring: List[String] = arstring.toList

    def arint: Array[Int] = parse(string).flatMap(_.as[Array[Int]]).toTry.get

    def lint: List[Int] = arint.toList
  }

  final class ArarOps[T](val arr: Array[Array[T]]) extends AnyVal {
    def toLilist = arr.map(_.toList).toList
  }
}
