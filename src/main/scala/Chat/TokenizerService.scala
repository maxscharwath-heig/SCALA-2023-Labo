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
  // TODO - Part 1 Step 3
  def tokenize(input: String): Tokenized = {
    // Remove punctuation
    val sanitizedTokens =
      input.replaceAll("[.,!?*]", "").replaceAll("[']", " ").split(" ")

    // Check if token is in dict
    val tokens: Array[(String, Chat.Token)] = sanitizedTokens.map(token => {
      // Get the word that ensures it exists in the dict
      val word = spellCheckerSvc.getClosestWordInDictionary(token)
      (word, getDictionnaryToken(word))
    })

    return new TokenizedImpl(tokens)
  }

  // Get the corresponding dictionnary token from a word
  private def getDictionnaryToken(word: String): Token = {
    word match {
      case "bonjour"                => Token.BONJOUR
      case "je"                     => Token.JE
      case "etre"                   => Token.ETRE
      case "vouloir"                => Token.VOULOIR
      case "assoiffe"               => Token.ASSOIFFE
      case "affame"                 => Token.AFFAME
      case "biere"                  => Token.PRODUIT
      case "croissant"              => Token.PRODUIT
      case "et"                     => Token.ET
      case "ou"                     => Token.OU
      case "svp"                    => Token.SVP
      case w if w.startsWith("_")   => Token.PSEUDO
      case w if w.forall(_.isDigit) => Token.NUM
      case _                        => Token.UNKNOWN
    }
  }

end TokenizerService
