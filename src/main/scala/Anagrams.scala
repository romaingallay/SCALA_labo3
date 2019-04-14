import scala.io.Source
import scala.collection.immutable._

/** Modified by Romain Gallay & Walid Koubaa **/

object Anagrams extends App {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** A fingerprint is a string which represents a sorted sequence of characters:
   *  Examples: 
   *
   *    "aaccx"
   *    "abyz"
   *    "ppp"
   *    ""
   */

  type FingerPrint = String


  /** The dictionary is simply a sequence of words.
   *  You can begin your development with this simple example. 
   *  A dictionary of English words is given to you as an external file (linuxwords.txt)  
   *  that you can load to use with your program  
   */

  val dictionaryTest: List[Word] =
    List("ate", "eat", "tea", "pot", "top", "sonja", "jason", "normal",
         "I", "love", "you", "olive")

  val dictionary = loadDictionary()

  def loadDictionary():List[Word] = {
    Source.fromFile("linuxwords.txt").getLines.toList   // open the linuxwords.txt dictionnary file
  }

  /** Converts a word/sentence into its fingerprint.
   *  The fingerprint has the same characters as the word, with the same
   *  number of occurrences, but the characters appear in sorted order.
   */

  def fingerPrint(s: Word): FingerPrint = s.toLowerCase sorted      //sort the Word
  def fingerPrint(s: Sentence): FingerPrint = fingerPrint(s.reduceLeft(_ + _))  // sort all the sentence words and return
                                                                                // the Fingerprint

  /** `matchingWords` is a `Map` from fingerprints to a sequence of all
   *  the words that have that fingerprint.
   *  This map serves as an easy way to obtain all the anagrams of a word given its fingerprint.
   *
   *  For example, the word "eat" has the fingerprint "aet".
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `matchingWords` map will contain an entry:
   *
   *   "aet"-> List("ate", "eat", "tea")
   */

  val matchingWords: Map[FingerPrint, List[Word]] = dictionary.groupBy(word => fingerPrint(word)).withDefaultValue(List())


  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = matchingWords(fingerPrint(word))


  // Test code with for example:
   println(wordAnagrams("eta"))
   println(wordAnagrams("jbdikb"))


  /** Returns the list of all subsequences of a fingerprint.
   *  This includes the fingerprint itself, i.e.
   *  "ko" is a subsequence of "kkoo". It also always includes
   *  the empty string "".
   *
   *  Example: the subsequences of the fingerprint "abbc" are
   *
   *    List("", "c", "b", "bc", "bb", "bbc", "a", "ac", "ab", "abc", "abb", "abbc")
   *
   *  Note that the order of the subsequences does not matter -- the subsequences
   *  in the example above could have been displayed in some other order.
   */

  def subseqs(fp: FingerPrint): List[FingerPrint] = {
    (fp.indices.flatMap(x => fp.combinations(x + 1)).toList :+ "").distinct
    // by default we append the empty string to the list of subsequences

  }

  // Test code with for example:
   println(subseqs("aabbc"))


  /** Subtracts fingerprint `y` from fingerprint `x`.
   *
   *  The precondition is that the fingerprint `y` is a subsequence of
   *  the fingerprint `x` -- any character appearing in `y` must
   *  appear in `x`.
   */


    def subtract(x: FingerPrint, y: FingerPrint): String = {
      x.toList.diff(y.toList).mkString    // substract all characters appearing in y from x
    }
    // Test code with for example:
    println(subtract("aabbcc", "abc"))     // should return "abc"
    println(subtract("a", "abc"))          // should return ""
    println(subtract("xyz", "z"))          // should return xy
    println(subtract("xxxxyzzzz", "xxzz")) // should return xxyzz
    println(subtract("", "xxzz"))          // should return ""
    println(subtract("", "abc"))           // should return ""


    /** Returns a list of all anagram sentences of the given sentence.
      *
      * An anagram of a sentence is formed by taking the fingerprint of all the characters of
      * all the words in the sentence, and producing all possible combinations of words with those characters,
      * such that the words have to be from the dictionary.
      *
      * The number of words in the sentence and its anagrams does not have to correspond.
      * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
      *
      * Also, two sentences with the same words but in a different order are considered two different anagrams.
      * For example, sentences `List("You", "olive")` and `List("olive","you")` are different anagrams of
      * `List("I", "love", "you")`.
      *
      * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
      * so it has to be returned in this list.
      *
      * Note: There is only one anagram of an empty sentence.
      */
    def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

      def loop(fp: FingerPrint): List[Sentence] = fp match {
        // trivial case
        case r if r.isEmpty => List(List())
        case _ => for {
          subseq <- subseqs(fp)
          anagram <- wordAnagrams(subseq) // take the anagrams from the subsequences of the fingerprint
          xs <- loop(subtract(fp, subseq)) // recursive call for the rest of the subsequence
        } yield anagram::xs
      }
      loop(fingerPrint(sentence))
    }
  // Test code with for example:
   println(sentenceAnagrams(List("eat", "tea")))
   println(sentenceAnagrams(List("you", "olive")))
   println(sentenceAnagrams(List("I", "love", "you")))
   println(sentenceAnagrams(List("jason", "love", "you")))

}