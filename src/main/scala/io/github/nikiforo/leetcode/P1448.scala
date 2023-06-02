package io.github.nikiforo.leetcode

object P1448 {
  
  private class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }
 
  def goodNodes(root: TreeNode): Int =
    go(root, Int.MinValue)

  private def go(node: TreeNode, max: Int): Int =
    if(node == null) 0
    else {
      val v = node.value
      if(v < max) go(node.left, max) + go(node.right, max)
      else 1 + go(node.left, v) + go(node.right, v)
    }
}
