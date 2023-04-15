// SCALA - Labo 2
// Nicolas Crausaz & Maxime Scharwath

package Chat

import Chat.Token.*
import Utils.SpellCheckerService
import Utils.Dictionary

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /** Separate the user's input into tokens
    * @param input
    *   The user's input
    * @return
    *   A Tokenizer which allows iteration over the tokens of the input
    */

  def tokenize(input: String): Tokenized = {
    // Remove punctuation
    val sanitizedTokens = input
      .replaceAll("[.,!?*']+", " ")
      .trim
      .split("\\s+")

    // For every word, get the closest word in the dict and the corresponding token
    val tokens: Array[(String, Chat.Token)] = sanitizedTokens.map(token => {
      val word = spellCheckerSvc.getClosestWordInDictionary(token)
      (word, getDictionnaryToken(word))
    })

    return new TokenizedImpl(tokens)
  }

  /** Get the corresponding token from a word of the dictionnary
    *
    * @param word
    *   The word of the dictionnary
    * @return
    *   The corresponding token
    */
  private def getDictionnaryToken(word: String): Token = {
    word match {
      case "bonjour"                            => Token.BONJOUR
      case "je"                                 => Token.JE
      case "etre"                               => Token.ETRE
      case "vouloir"                            => Token.VOULOIR
      case "assoiffe"                           => Token.ASSOIFFE
      case "affame"                             => Token.AFFAME
      case "biere"                              => Token.PRODUIT
      case "croissant"                          => Token.PRODUIT
      case "et"                                 => Token.ET
      case "ou"                                 => Token.OU
      case "svp"                                => Token.SVP
      case "quel"                               => Token.QUEL
      case "le"                                 => Token.LE
      case "prix"                               => Token.PRIX
      case "de"                                 => Token.DE
      case "combien"                            => Token.COMBIEN
      case "couter"                             => Token.COUTER
      case "commander"                          => Token.COMMANDER
      case "connaître"                          => Token.CONNAITRE
      case "mon"                                => Token.MON
      case "me"                                 => Token.ME
      case "solde"                              => Token.SOLDE
      case "appeler"                            => Token.APPELLER
      case "marque"                             => Token.MARQUE
      case "maison"                             => Token.MARQUE
      case "cailler"                            => Token.MARQUE
      case "farmer"                             => Token.MARQUE
      case "boxer"                              => Token.MARQUE
      case "wittekop"                           => Token.MARQUE
      case "punkipa"                            => Token.MARQUE
      case "jackhammer"                         => Token.MARQUE
      case "tenebreuse"                         => Token.MARQUE
      case w if spellCheckerSvc.isAPseudonym(w) => Token.PSEUDO
      case w if spellCheckerSvc.isANumber(w)    => Token.NUM
      case _                                    => Token.UNKNOWN
    }
  }
end TokenizerService
