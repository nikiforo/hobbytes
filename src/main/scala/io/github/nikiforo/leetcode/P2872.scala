package io.github.nikiforo.leetcode

object P2872 {

  import scala.collection.mutable

  def maxKDivisibleComponents(n: Int, edges: Array[Array[Int]], values: Array[Int], k: Int): Int = {
    val map = mutable.Map.empty[Int, Set[Int]]
    edges.foreach { case Array(v1, v2) =>
      map(v1) = map.getOrElse(v1, Set.empty) + v2
      map(v2) = map.getOrElse(v2, Set.empty) + v1
    }
    var components = 0
    val stack = mutable.Stack.from(map.collect { case (v, set) if set.size == 1 => v })
    while (stack.nonEmpty) {
      val v = stack.pop()
      if (values(v) % k == 0) components += 1
      map(v).foreach { n =>
        values(n) = (values(n) + values(v)) % k
        map(n) -= v
        if (map(n).size == 1) stack.push(n)
      }
    }
    math.max(components, 1)
  }
}
