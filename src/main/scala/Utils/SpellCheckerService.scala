package Utils

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Calculate the Levenstein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenstein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number or a pseudonym, this function just returns it.
    * @param misspelledWord the mispelled word to correct
    * @return the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String]) extends SpellCheckerService:
  // Compute the Levenstein distance between two words
  def stringDistance(s1: String, s2: String): Int =
    ((0 to s2.length).toList /: s1.toList) { (prev, c1) =>
      (prev zip prev.tail zip s2.toList).scanLeft(prev.head + 1) {
        case (h, ((d, v), c2)) => Math.min(Math.min(h + 1, v + 1), d + (if (c1 == c2) 0 else 1))
      }
    }.last
  def getClosestWordInDictionary(misspelledWord: String): String = dictionary.keys.minBy(stringDistance(misspelledWord, _))
end SpellCheckerImpl
