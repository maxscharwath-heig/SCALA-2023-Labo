package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg) {}

class Parser(tokenized: Tokenized):
  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an
    * error.
    */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts
    * arbitrarily many arguments of type Token
    */
  private def expected(token: Token, more: Token*): Nothing =
    expected(more.prepended(token))
  private def expected(tokens: Seq[Token]): Nothing =
    val expectedTokens = tokens.mkString(" or ")
    throw new UnexpectedTokenException(
      s"Expected: $expectedTokens, found: $curToken"
    )

  private def parseCommand() = {

  }

  /** the root method of the parser: parses an entry phrase */
  // TODO - Part 2 Step 4
  def parsePhrases(): ExprTree =

    // BONJOUR (optional)
    if curToken == BONJOUR then readToken()

    // QUEL EST LE PRIX DE
    if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
      // TODO: Command
      Solde
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      // TODO: Command
      Solde

    // JE
    else if curToken == JE then
      readToken()
      // VOULOIR [COMMANDER | CONNAITRE MON SOLDE]
      if curToken == VOULOIR then
        readToken()
        if curToken == COMMANDER then
          // todo: command
          Solde
        else if curToken == CONNAITRE then
          readToken()
          eat(MON)
          eat(SOLDE)
          Solde
        else expected(COMMANDER, CONNAITRE)

      // ETRE [ASSOIFFE | AFFAME | PSEUDO]
      else if curToken == ETRE then
        readToken()
        if curToken == ASSOIFFE then Thirsty
        else if curToken == AFFAME then Hungry
        else if curToken == PSEUDO then
          val pseudo = eat(PSEUDO)
          Auth(pseudo)
        else expected(ASSOIFFE, AFFAME, PSEUDO)

      // ME APPELLER PSEUDO
      else if curToken == ME then
        eat(APPELLER)
        val pseudo = eat(PSEUDO)
        Auth(pseudo)
      else expected(QUEL, COMBIEN, JE)
    else expected(VOULOIR, ETRE, JE)
