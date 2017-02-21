package patmat
import scala.collection.mutable.{Map, SynchronizedMap, HashMap}
import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  
 def print2(tree: CodeTree, s: String): Unit = tree match {
    case Leaf(c, w) => println(s + "Leaf: ", c, w)
    case Fork(left, right, c, w) => {
      println(s + "Fork: ", c, w);
      print2(left, "left")
      print2(right, "right")
    }
  }
  
  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Leaf(c, w) => w
    case Fork(left, right, c, w) => weight(left) + weight(right)
  }
  
  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, w) => List(c)
    case Fork(left, right, c, w) => chars(left) ++ chars(right) 
  }
  
  def contains(tree: CodeTree, input: Char): Boolean = tree match {
    case Leaf(c, w) => c == input
    case Fork(left, right, c, w) => c.contains(input)
  }
  
  def makeCodeTree(left: CodeTree, right: CodeTree) = {
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
  }

  def lt(left: CodeTree, right: CodeTree): Boolean = weight(left) <= weight(right)
  
  def insert(list: List[CodeTree], t: CodeTree) : List[CodeTree] = list match {
   case Nil => List[CodeTree](t)
   case x::r if(lt(t,x)) => t::x::r
   case x::r => x :: insert(r,t)
  }
  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
   
  def times(chars: List[Char]) : List[(Char, Int)] = {
    val m = Map[Char, Int]()
    
    for (i <- chars) {
      m.get(i) match {
        case None => m(i) = 1
        case Some(v) => m(i) = v+1
      }
    }
    
    var l = List[(Char, Int)]()
    for ((k,v) <- m) l = l ++ List((k, v))
    l
  }
    
 /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
   def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
     val f = freqs.sortBy(x => x._2)
     for (i <- f) yield Leaf(i._1, i._2)
   }
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
    def singleton(trees: List[CodeTree]): Boolean = trees.length == 1
    
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
    def combine(trees: List[CodeTree]): List[CodeTree] = 
      if (trees.length < 2) trees else insert(trees.tail.tail, makeCodeTree(trees.head, trees.tail.head))
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
   def until(unittree: List[CodeTree] => Boolean, callcombine: List[CodeTree] => List[CodeTree])(hufInput: List[CodeTree]): List[CodeTree] =
     if (unittree(hufInput)) hufInput else until(unittree, callcombine)(callcombine(hufInput))
  
  /**	
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
    def createCodeTree(chars: List[Char]): CodeTree = 
      (until(singleton, combine)(makeOrderedLeafList(times(chars)))).head

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    
    def decodeImpl(tree1: CodeTree, bits1: List[Bit]) : List[Char] = tree1 match {
      
      case Leaf(c, w) => if (bits1 != Nil) List(c) ++ decode(tree, bits1) else List(c)
      case Fork(left, right, c, w) => {
        if (bits1 != Nil) {
          if (bits1.head == 0) decodeImpl (left, bits1.tail) else decodeImpl(right, bits1.tail)
        } else List[Char]()
      }
    }
    decodeImpl(tree, bits)
  }
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)
  def encodedM: List[Bit] = encode(frenchCode)(decodedSecret)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
   def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeImpl(tree1: CodeTree, input: Char) : List[Bit] = tree1 match {
      case Leaf(c, w) => List[Bit]()
      case Fork(left, right, c, w) => {
          if (contains(left, input)) 0 :: encodeImpl(left, input)
          else 1 :: encodeImpl(right, input)
      }
    }
    var v = List[Bit]()
    for (c <- text) v = v ++ encodeImpl(tree, c)
    v
  }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
   def codeBits(table: CodeTable)(char: Char): List[Bit] = table.find(_._1 == char).get._2
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
   def convert(tree: CodeTree): CodeTable = tree match {
      case Leaf(c, w) => List((c, List[Bit]()))
      case Fork(left, right, c, w) => mergeCodeTables(convert(left), convert(right))
  }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
   def mergeCodeTables(l: CodeTable, r: CodeTable): CodeTable = 
     l.map {case (x, y) => (x, 0 :: y)} ++ r.map {case (x, y) => (x, 1 :: y)}
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
   def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
     def quickEncodeImpl(table: CodeTable, text: List[Char]) : List[Bit] = text match {
       case Nil => List[Bit]()
       case x::xs => table.find(_._1 == text.head).get._2 ::: quickEncodeImpl(table, text.tail)
     }
     quickEncodeImpl(convert(tree), text)
   }

   object test {
     def execute() {
       /*val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
       val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
       println("weight of t1 is " + weight(t1))
       println("chars of t2 is " + chars(t2))
       println(string2Chars("hello, world"))
       println(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
       val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
       println(combine(leaflist))
       println(encode(t1)("abbbbababbbbababbbbababbbbababbbbab".toList))
       println(decode(t1, encode(t1)("abbbbababbbbababbbbababbbbababbbbab".toList)))
       println(secret)
       println(decodedSecret)
       println(encodedM)
       assert(encodedM == secret)*/
       /*val ct = createCodeTree("aabbbcddddddeeefffffff".toList)
       println("Weight: ", weight(ct))
       println("Chars: ", chars(ct))
       println("Times: ", times("aabbbcddddddeeefffffff".toList))
       val t = times("aabbb".toList)
       val s = makeOrderedLeafList(t)
       s.foreach { case Leaf(x,y) => println(x, y) }
       val f = createCodeTree("aabbbcddddddeeefffffff".toList)
       print2(f, "root")
       //println("Encode(a) ", encode(ct)(List('a')))
       println(decode(ct, encode(ct)("aabbb".toList)))
       println(decode(ct, encode(ct)("abbbbababbbbababbbbababbbbababbbbab".toList)))
       println("************")
       println(decode(ct, quickEncode(ct)("abbbbababbbbababbbbababbbbababbbbab".toList)))*/
       
       /*println("11111")
       val w1 = combine(List(Leaf('x',4), Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
       for (i <- w1) print2(i, "HELLO")
       val w2 = combine(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
       for (i <- w2) print2(i, "HELLO2")
       val ss = "all letters? try, the quick brown fox jumps over the lazy dog. ";
       val ct2 = createCodeTree(ss.toList)
       val xx = convert(ct2)
       println(encode(ct2)(ss.toList))*/
       val ff = "You're my man, my mighty king,\nAnd I'm the jewel in your crown,\nYou're the sun so hot and bright,\nI'm your light-rays shining down,\n\nYou're the sky so vast and blue,\nAnd I'm the white clouds in your chest,\nI'm a river clean and pure,\nWho in your ocean finds her rest,\n\nYou're the mountain huge and high,\nI'm the valley green and wide,\nYou're the body firm and strong,\nAnd I'm a rib bone on your side,\n\nYou're an eagle flying high,\nI'm your feathers light and brown,\nYou're my man, my king of kings,\nAnd I'm the jewel in your crown.\n\nNima Akbari"
       val ct3 = createCodeTree("You're my man, my mighty king".toList)
       //println(encode(ct3)(ff.toList))
       val yy = convert(ct3)
       for (i <- yy) println(i._1, i._2)
       println(makeOrderedLeafList(times("You're my man, my mighty king".toList)))
     }
   }
}
   
 object Main extends App {  
   println("START")
   Huffman.test.execute()
   println("DONE")
 }
 
