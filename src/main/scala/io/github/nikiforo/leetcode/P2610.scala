package io.github.nikiforo.leetcode

object P2610 {

  def findMatrix(nums: Array[Int]): List[List[Int]] = {
    var vec = Vector.empty[List[Int]]
    nums.foreach { i =>
      val ind = vec.indexWhere(!_.contains(i))
      if (ind == -1) vec = vec :+ List(i)
      else vec = vec.updated(ind, i :: vec(ind))
    }
    vec.toList
  }
}
