package io.github.nikiforo.leetcode

object P210 {

  import scala.collection.mutable

  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    val reqMap = mutable.Map.empty[Int, Set[Int]]
    prerequisites.foreach { case Array(c, req) => reqMap(c) = reqMap.getOrElse(c, Set.empty) + req }
    val courses = mutable.Stack.from((0 until numCourses).filter(c => !reqMap.contains(c)))
    val result = Array.newBuilder[Int]
    while(courses.nonEmpty) {
      val c = courses.pop()
      result.addOne(c)
      reqMap.foreach { case (k, set) =>
        if (set != Set(c)) reqMap(k) = set - c
        else {
          reqMap.remove(k)
          courses.push(k)
        }
      }
    }
    if (reqMap.isEmpty) result.result() else Array.empty
  }
}
