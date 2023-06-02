package io.github.nikiforo.leetcode

import scala.collection.immutable.Queue

object P90 {

  def subsetsWithDup(nums: Array[Int]): List[List[Int]] =
    nums.sorted.foldLeft(Set(List.empty[Int]))((s, n) => s ++ s.map(n :: _)).toList
}
