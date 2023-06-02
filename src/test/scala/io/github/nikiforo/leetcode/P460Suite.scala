package io.github.nikiforo.leetcode

import org.scalatest.DoNotDiscover

@DoNotDiscover
final class P460Suite extends LeetcodeSuite {

  test("1") {
    val  lfu = new P460.LFUCache(2);
    lfu.put(1, 1);   // cache=[1,_], cnt(1)=1
    lfu.put(2, 2);   // cache=[2,1], cnt(2)=1, cnt(1)=1
    assert(lfu.get(1) == 1)      // return 1
                    // cache=[1,2], cnt(2)=1, cnt(1)=2
    lfu.put(3, 3);   // 2 is the LFU key because cnt(2)=1 is the smallest, invalidate 2.
                    // cache=[3,1], cnt(3)=1, cnt(1)=2
    assert(lfu.get(2) == -1)      // return -1 (not found)
    assert(lfu.get(3) == 3)      // return 3
                    // cache=[3,1], cnt(3)=2, cnt(1)=2
    lfu.put(4, 4);   // Both 1 and 3 have the same cnt, but 1 is LRU, invalidate 1.
                    // cache=[4,3], cnt(4)=1, cnt(3)=2
    assert(lfu.get(1) == -1)      // return -1 (not found)
    assert(lfu.get(3) == 3)      // return 3
                    // cache=[3,4], cnt(4)=1, cnt(3)=3
    assert(lfu.get(4) == 4)      // return 4
                    // cache=[4,3], cnt(4)=2, cnt(3)=3
  }

  test("2") {
    val  lfu = new P460.LFUCache(2);
    lfu.put(3, 1);   
    lfu.put(2, 1);   
    lfu.put(2, 2);   
    lfu.put(4, 4);   
    assert(lfu.get(2) == 2)
  }

  test("3") {
    val lfu = new P460.LFUCache(3)
    lfu.put(1, 1)
    lfu.put(2, 2)
    lfu.put(3, 3)
    lfu.put(4, 4)
    assert(lfu.get(4) == 4)
    assert(lfu.get(3) == 3)
    assert(lfu.get(2) == 2)
    assert(lfu.get(1) == -1)
    lfu.put(5, 5)
    assert(lfu.get(1) == -1)
    assert(lfu.get(2) == 2)
    assert(lfu.get(3) == 3)
    assert(lfu.get(4) == -1)
    assert(lfu.get(5) == 5)
  }
}
