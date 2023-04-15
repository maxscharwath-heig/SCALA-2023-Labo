package Chat

import scala.quoted.Expr

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

  private def parseProduct(): ExprTree = {
    // set default values for quantity and brand
    val quantity = if curToken == NUM then eat(NUM).toInt else 1
    val name = if curToken == PRODUIT then eat(PRODUIT) else ""
    val brand = if curToken == MARQUE then eat(MARQUE) else ""

    Product(name, brand, quantity)
  }

  private def parseCommand(): ExprTree = leftAssocOp(parseProduct())

  // Parse Pseudonym and remove the leading "_"
  private def parsePseudonym(): ExprTree = Auth(eat(PSEUDO).substring(1)) 

  private def leftAssocOp(expr: ExprTree): ExprTree = curToken match
    case ET =>
      readToken()
      leftAssocOp(And(expr, parseCommand()))
    case OU =>
      readToken()
      leftAssocOp(Or(expr, parseCommand()))
    case _ => expr

  // Helper to parse with the given tokens and then parse with the given parser
  private def parseWith(tokens: Token*)(parser: => ExprTree): ExprTree = {
    tokens.foreach(eat)
    parser
  }

  // the root method of the parser: parses an entry phrase
  def parsePhrases(): ExprTree = {
    if curToken == BONJOUR then readToken() // Optional BONJOUR

    curToken match
      case QUEL =>
        readToken()
        parseWith(ETRE, LE, PRIX, DE)(Price(parseCommand()))
      case COMBIEN =>
        readToken()
        parseWith(COUTER)(Price(parseCommand()))
      case JE =>
        readToken()
        curToken match
          case VOULOIR => parseWith(VOULOIR)(parseWantPhrase())
          case ETRE => parseWith(ETRE)(parseBePhrase())
          case ME => parseWith(ME, APPELLER)(parseMePhrase())
          case _ => expected(VOULOIR, ETRE, ME)
      case _ => expected(QUEL, COMBIEN, JE)
  }

  // VOULOIR [COMMANDER | CONNAITRE MON SOLDE]
  private def parseWantPhrase(): ExprTree = curToken match
    case COMMANDER =>
      readToken()
      Command(parseCommand())
    case CONNAITRE =>
      parseWith(CONNAITRE, MON, SOLDE)(Solde)
    case _ => expected(COMMANDER, CONNAITRE)

  // ETRE [ASSOIFFE | AFFAME | PSEUDO]
  private def parseBePhrase(): ExprTree = curToken match
    case ASSOIFFE =>
      readToken()
      Thirsty
    case AFFAME =>
      readToken()
      Hungry
    case PSEUDO => parsePseudonym()
    case _ => expected(ASSOIFFE, AFFAME, PSEUDO)
  
  // ME APPELLER PSEUDO
  private def parseMePhrase(): ExprTree = {
    val pseudo = eat(PSEUDO)
    Auth(pseudo)
  }

end Parser