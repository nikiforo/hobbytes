package me.nikiforo.leetcode

import io.circe.parser.parse
import io.circe.syntax._
import me.nikiforo.leetcode.ArrayOps._

trait ArrayOps {

  implicit def argumentsOps(string: String) =
    new ArrayStringOps(string)

  implicit def ararOps[T](arr: Array[Array[T]]) =
    new ArarOps(arr)
}

object ArrayOps {

  final class ArrayStringOps(val string: String) extends AnyVal {

    def ararint = parse(string).flatMap(_.as[Array[Array[Int]]]).getOrElse(???)

    def lilint = ararint.map(_.toList).toList

    def arstring = parse(string).flatMap(_.as[Array[String]]).getOrElse(???)

    def arint = parse(string).flatMap(_.as[Array[Int]]).getOrElse(???)

    def lint = arint.toList
  }

  final class ArarOps[T](val arr: Array[Array[T]]) extends AnyVal {
    def toLilist = arr.map(_.toList).toList
  }
}
