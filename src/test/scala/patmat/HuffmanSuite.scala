package patmat

import scala.::

class HuffmanSuite extends munit.FunSuite :

  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree (10pts)") {
    new TestTrees :
      assertEquals(weight(t1), 5)
      assertEquals(weight(t2), 9)
  }

  test("insert test") {
    new TestTrees :
      assertEquals(times(List('a', 'b', 'c', 'a', 'b', 'c')), List(('a', 2), ('b', 2), ('c', 2)))
      assertEquals(times(List('a', 'b', 'c', 'a', 'c', 'c')), List(('a', 2), ('b', 1), ('c', 3)))
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees :
      assertEquals(chars(t2), List('a', 'b', 'd'))
      assertEquals(chars(t1), List('a', 'b'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until") {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val lists: List[CodeTree] = List(t1) ::: List(t2) ::: List(frenchCode)

    val listTree: List[CodeTree] = until(x => true, combine)(lists)
    val listTree2: List[CodeTree] = until(x => false, x => (List(t2)))(lists)
    val listTree3: List[CodeTree] = until(x => true, x => (List(t2)))(lists)
    val listTree4: List[CodeTree] = until(x => false, combine)(lists)

    assertEquals(listTree, listTree4)
    assertEquals(listTree2, listTree3)
  }

  test("createCodeTree") {
    assertEquals(createCodeTree(List('a', 'b', 'a')), Fork(Leaf('b', 1), Leaf('a', 2), List('b', 'a'), 3))
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    assertEquals(makeOrderedLeafList(List(('t', 2), ('q', 5), ('z', 6), ('o', 7), ('w', 8), ('e', 4), ('x', 3))),
      List(Leaf('t', 2), Leaf('x', 3), Leaf('e', 4), Leaf('q', 5), Leaf('z', 6), Leaf('o', 7), Leaf('w', 8)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }
  
  test("secretSentance") {
    assertEquals(decodedSecret, List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }
  
  test("codeBits") {
    val tree = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val codeTable: CodeTable = convert(tree)
    assertEquals(codeBits(codeTable)('a'), List(0))
    assertEquals(codeBits(codeTable)('b'), List(1))
  }

  test("encode using french code") {
    assertEquals(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)), "huffmanestcool".toList)
  }

  test("encode and quickEncode same results") {
    val a: List[Bit] = quickEncode(frenchCode)("huffmanestcool".toList)
    val b: List[Bit] = encode(frenchCode)("huffmanestcool".toList)
    assertEquals(a, b)
  }

  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds
