package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P212Suite extends LeetcodeSuite {

  test("1") {
    val board = """[["o","a","a","n"],["e","t","a","e"],["i","h","k","r"],["i","f","l","v"]]""".ararchar
    val words = """["oath","pea","eat","rain"]""".arstring
    assert(P212.findWords(board, words).toList == """["oath","eat"]""".listring)
  }

  test("2") {
    val board = """[["a"]]""".ararchar
    val words = """["a"]""".arstring
    assert(P212.findWords(board, words).toList == """["a"]""".listring)
  }

}
