// SCALA - Labo 1
// Nicolas Crausaz & Maxime Scharwath

package Utils

trait SpellCheckerService:
  /** This dictionary is a Map object that contains valid words as keys and
    * their normalized equivalents as values (e.g. we want to normalize the
    * words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /** Calculate the Levenstein distance between two words.
    * @param s1
    *   the first word
    * @param s2
    *   the second word
    * @return
    *   an integer value, which indicates the Levenstein distance between "s1"
    *   and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /** Get the syntactically closest word in the dictionary from the given
    * misspelled word, using the "stringDistance" function. If the word is a
    * number or a pseudonym, this function just returns it.
    * @param misspelledWord
    *   the mispelled word to correct
    * @return
    *   the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String

  /** Check if the given word is a pseudonym.
    * @param word
    *   the word to check
    * @return
    *   true if the word is a pseudonym, false otherwise
    */
  def isAPseudonym(word: String): Boolean

  /** Check if the given word is a number.
    * @param word
    *   the word to check
    * @return
    *   true if the word is a number, false otherwise
    */
  def isANumber(word: String): Boolean

end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String])
    extends SpellCheckerService:

  def isAPseudonym(word: String): Boolean = word.startsWith("_")

  def isANumber(word: String): Boolean = word.forall(_.isDigit)

  // Compute the Levenstein distance between two words
  def stringDistance(s1: String, s2: String): Int =
    (s1, s2) match
      case ("", s2) => s2.length
      case (s1, "") => s1.length
      case (s1, s2) =>
        val cost = if s1.last == s2.last then 0 else 1
        Math.min(
          Math.min(
            stringDistance(s1.init, s2) + 1,
            stringDistance(s1, s2.init) + 1
          ),
          stringDistance(s1.init, s2.init) + cost
        )

  def getClosestWordInDictionary(misspelledWord: String): String = {
    // If the word is a number or a pseudo, we just return it
    if (isANumber(misspelledWord) || isAPseudonym(misspelledWord)) {
      return misspelledWord
    }

    val closest = dictionary
      .foldLeft(("", Int.MaxValue))((acc, elem) => {
        val (key, value) = elem
        val (closestKey, closestDist) = acc
        val dist = stringDistance(key, misspelledWord)

        // Smaller distance, or egality: we get the first key sorted by alpha
        if dist < closestDist || (dist == closestDist && key < closestKey) then
          (key, dist)
        else acc
      })
      ._1

    return dictionary(closest)
  }
end SpellCheckerImpl
