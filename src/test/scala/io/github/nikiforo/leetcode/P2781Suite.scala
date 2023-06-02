package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
class P2781Suite extends LeetcodeSuite {

  test("""word = "cbaaaabc", forbidden = ["aaa","cb"]""") {
    val actual = P2781.longestValidSubstring("cbaaaabc", """["aaa","cb"]""".listring)
    assert(actual == 4)
  }

  test("""word = "leetcode", forbidden = ["de","le","e"]""") {
    val actual = P2781.longestValidSubstring("leetcode", """["de","le","e"]""".listring)
    assert(actual == 4)
  }

  test(""""aaaabaaacc" ["bcca","aaa","aabaa","baaac"]""") {
    val actual = P2781.longestValidSubstring("aaaabaaacc","""["bcca","aaa","aabaa","baaac"]""".listring)
    assert(actual == 4)
  }

  test("c1") {
    val actual = P2781.longestValidSubstring("abaaacc","""["aaa","baaac"]""".listring)
    assert(actual == 4)
  }

  test(""""bcac" ["bcac","caca","bcac","bca"]""") {
    val actual = P2781.longestValidSubstring("bcac", """["bcac","caca","bcac","bca"]""".listring)
    assert(actual == 3)
  }
}
