package me.nikiforo.leetcode

object P15 {

  def threeSum(arr: Array[Int]): List[List[Int]] = {
    val nums = arr.sorted

    def bin(l: Int, r: Int, num: Int): Boolean =
      if (l > r) false
      else {
        val m = (l + r) / 2
        nums(m) == num || (if (nums(m) < num) bin(m + 1, r, num) else bin(l, m - 1, num))
      }

    def next(i: Int) = (i + 1 until nums.length).find(j => nums(j) != nums(i))

    def go(i: Int, j: Int, acc: List[List[Int]]): List[List[Int]] =
      if (j >= nums.length) next(i).fold(acc)(ind => go(ind, ind + 1, acc))
      else {
        val search = 0 - nums(i) - nums(j)
        val found = bin(j + 1, nums.length - 1, search)
        val nAcc = if (found) List(nums(i), nums(j), search) :: acc else acc
        go(i, next(j).getOrElse(Int.MaxValue), nAcc)
      }

    go(0, 1, List.empty)
  }
}
