package io.github.nikiforo.leetcode

object P2 {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val result = new ListNode(0)
    go(result, l1, l2, 0)
    result.next
  }

  private def go(link: ListNode, l1: ListNode, l2: ListNode, acc: Int): Unit = {
    val nums = List(l1, l2).filter(_ != null)
    if (nums.isEmpty && acc > 0) link.next = new ListNode(acc)
    else if (nums.nonEmpty) {
      val sum = nums.map(_.x).sum + acc
      link.next = new ListNode(sum % 10)
      if (nums.size > 1) go(link.next, nums(0).next, nums(1).next, sum / 10)
      else go(link.next, nums(0).next, null, sum / 10)
    }
  }
}
