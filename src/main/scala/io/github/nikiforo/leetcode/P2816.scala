package io.github.nikiforo.leetcode

object P2816 {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def doubleIt(head: ListNode): ListNode = {
    val doubled = go(new ListNode(0, head))
    if (doubled.x == 0) doubled.next else doubled
  }

  private def go(node: ListNode): ListNode =
    if (node.next == null) new ListNode(node.x * 2 % 10, null)
    else new ListNode(node.x * 2 % 10 + node.next.x * 2 / 10, go(node.next))
}
