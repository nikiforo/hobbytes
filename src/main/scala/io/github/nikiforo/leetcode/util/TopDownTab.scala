package io.github.nikiforo.leetcode.util

abstract class TopDownTab[S, O](tab: scala.collection.mutable.Map[S, O]) {

  final def compute(s: S): O = tab.getOrElseUpdate(s, bare(s))

  protected def bare(s: S): O
}
