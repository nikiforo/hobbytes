package io.github.nikiforo.leetcode

object P207 {

  import scala.collection.mutable

  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    val reqMap = mutable.Map.empty[Int, Set[Int]]
    prerequisites.foreach { case Array(c, req) => reqMap(c) = reqMap.getOrElse(c, Set.empty) + req }
    val courses = mutable.Stack.from((0 until numCourses).filter(c => !reqMap.contains(c)))
    while(courses.nonEmpty) {
      val c = courses.pop()
      reqMap.foreach { case (k, set) =>
        if (set != Set(c)) reqMap(k) = set - c
        else {
          reqMap.remove(k)
          courses.push(k)
        }
      }
    }
    reqMap.isEmpty
  }

  private val init = Map.empty[Int, Set[Int]]

  def canFinishImmutable(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    val pMap = prerequisites.foldLeft(init)((m, p) => m.updated(p(0), m.getOrElse(p(0), Set.empty) + p(1)))
    goFinish((0 until numCourses).filter(c => !pMap.contains(c)).toList, pMap)
  }

  private def goFinish(courses: List[Int], pMap: Map[Int, Set[Int]]): Boolean =
    (pMap.isEmpty, courses) match {
      case (true, _) => true
      case (_, Nil) => false
      case (_, c :: tail) =>
        val (nCourses, nMap) = pMap.iterator.foldLeft((tail, pMap)) { case ((nCourses, m), (k, set)) =>
          if (set == Set(c)) (k :: nCourses, m.removed(k))
          else (nCourses, m.updated(k, set - c))
        }
        goFinish(nCourses, nMap)
    }
}
