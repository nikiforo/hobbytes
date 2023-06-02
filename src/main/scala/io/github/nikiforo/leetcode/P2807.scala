package io.github.nikiforo.leetcode

object P2807 {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def insertGreatestCommonDivisors(head: ListNode): ListNode = {
    inplace(head)
    head
  }

  private def inplace(head: ListNode): Unit = {
    val next = head.next
    if(next != null) {
      val n = new ListNode(gcd(head.x, next.x), next)
      head.next = n
      inplace(next)
    }
  }

  private def gcd(a: Int,b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}
