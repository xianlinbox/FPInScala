package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val t3 = Leaf('a', 2)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
      assert(weight(t3) === 2)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
      assert(chars(t2) === List('a', 'b', 'd'))
      assert(chars(t3) === List('a'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    List(('h', 1), ('e', 1), ('l', 3),
      ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1)).foreach((item) => {
      assert(times(string2Chars("hello, world")).contains(item))
    })
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of some leaf list 2") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 4), Leaf('x', 5))
    assert(combine(leaflist) === List(Leaf('x', 5), Fork(Leaf('e', 2), Leaf('t', 4), List('e', 't'), 6)))
  }

  test("decodeSecret") {
    println(decodedSecret.toString())
  }

  test("decode") {
    new TestTrees {
      assert(decode(t1, List(0)) == List('a'))
      assert(decode(t1, List(1)) == List('b'))
      assert(decode(t1, List(1, 0)) == List('b', 'a'))
      assert(decode(t1, List(0, 1)) == List('a', 'b'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode") {
    new TestTrees {
      assert(encode(t1)("a".toList) == List(0))
      assert(encode(t1)("b".toList) == List(1))
      assert(encode(t1)("ab".toList) == List(0, 1))
      assert(encode(t1)("ba".toList) == List(1, 0))
    }
  }

  test("codebits") {
    val table = List(('a', List(0)), ('b', List(1)))
    assert(codeBits(table)('a') == List(0))
    assert(codeBits(table)('b') == List(1))
  }
}
