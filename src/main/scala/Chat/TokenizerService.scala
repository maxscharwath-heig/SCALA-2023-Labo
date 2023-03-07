package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 1 Step 3
  def tokenize(input: String): Tokenized = {
    val rawTokens = input.split(" ")
    val tokens: Array[(String, Chat.Token)] = rawTokens.map(token => {
      // TODO: conditions
      (token, Token.BONJOUR)
    })

    return new TokenizedImpl(tokens)
  }
end TokenizerService
