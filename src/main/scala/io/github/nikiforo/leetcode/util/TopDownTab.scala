package io.github.nikiforo.leetcode.util

abstract class TopDownTab[S, O](tab: scala.collection.mutable.Map[S, O]) {

  final def compute(s: S): O = 
    tab.get(s).getOrElse {
      val result = bare(s)
      tab(s) = result
      result
    }

  protected def bare(s: S): O
}
