package io.github.nikiforo.leetcode

object P19 {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    val link = new ListNode(0, head)
    val node = go(link, (0 to n).foldLeft(link)((node, _) => node.next))
    node.next = node.next.next
    link.next
  }
    

  def go(front: ListNode, back: ListNode): ListNode = if (back == null) front else go(front.next, back.next)
}
