package io.github.nikiforo.leetcode

object P2806 {

  def accountBalanceAfterPurchase(purchaseAmount: Int): Int = 
    100 - (purchaseAmount + 5) / 10 * 10
}
