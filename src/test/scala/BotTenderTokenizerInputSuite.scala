import org.scalatest.*
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import prop.*

import java.io.ByteArrayOutputStream
import Utils.{Dictionary, SpellCheckerService, SpellCheckerImpl}
import Chat.{TokenizerService, Token}

class BotTenderTokenizerInputSuite
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with should.Matchers {
  val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(
    Dictionary.dictionary
  )
  val tokenizerSvc: TokenizerService = new TokenizerService(spellCheckerSvc)

  val evaluateInput = MainTokenizer.evaluateInput(tokenizerSvc)

  // You can use this test to debug any input
  property("inputting") {
    evaluateInput("quitter")
  }

  property("inputting 'quitter'") {
    // capture output for testing therefore it is not shown in the terminal
    val outCapture = new ByteArrayOutputStream
    Console.withOut(outCapture) {
      evaluateInput("quitter") should equal(false)
    }
    outCapture.toString() should include("Adieu.")
  }

  property("inputting 'santé !'") {
    evaluateInput("santé !") should equal(true)
  }

  property("String distance should be correct") {
    spellCheckerSvc.stringDistance("", "") should equal(0)
    spellCheckerSvc.stringDistance("santé", "") should equal("santé".length)
    spellCheckerSvc.stringDistance("", "santé") should equal("santé".length)
    spellCheckerSvc.stringDistance("santé", "santé") should equal(0)
    spellCheckerSvc.stringDistance("santy", "santé") should equal(1)
    spellCheckerSvc.stringDistance("santé", "santy") should equal(1)
    spellCheckerSvc.stringDistance("test", "testing") should equal(3)
    spellCheckerSvc.stringDistance("bière", "target") should equal(5)
  }

  property("Should get the closest word in dictonnary") {
    spellCheckerSvc.getClosestWordInDictionary("1234") should equal("1234")
    spellCheckerSvc.getClosestWordInDictionary("_Nicolas") should equal(
      "_Nicolas"
    )
    spellCheckerSvc.getClosestWordInDictionary("bonjour") should equal(
      "bonjour"
    )
    spellCheckerSvc.getClosestWordInDictionary("hello") should equal("bonjour")
    spellCheckerSvc.getClosestWordInDictionary("bonjor") should equal("bonjour")
    spellCheckerSvc.getClosestWordInDictionary("croissants") should equal(
      "croissant"
    )
    spellCheckerSvc.getClosestWordInDictionary("crsoiasabsdts") should equal(
      "croissant"
    )
  }

  property("Should return the same result from example (a)") {
    val expectedTokens: List[Chat.Token] = List(
      Token.JE,
      Token.VOULOIR,
      Token.NUM,
      Token.PRODUIT,
      Token.ET,
      Token.NUM,
      Token.PRODUIT,
      Token.SVP,
      Token.EOL
    )

    val input = "Je veux 12 bières et 4 croissants, stp."
    val tokens = tokenizerSvc.tokenize(input)

    // Loop over the tokens and check if they are the same as the expected tokens
    for (i <- 0 until expectedTokens.length) {
      tokens.nextToken()._2 should equal(expectedTokens(i))
    }
  }

  property("Should return the same result from example (b)") {
    val expectedTokens: List[Chat.Token] = List(
      Token.JE,
      Token.VOULOIR,
      Token.NUM,
      Token.PRODUIT,
      Token.SVP,
      Token.EOL
    )

    val input = "J'aimerais 2 bières stp !"
    val tokens = tokenizerSvc.tokenize(input)

    // Loop over the tokens and check if they are the same as the expected tokens
    for (i <- 0 until expectedTokens.length) {
      tokens.nextToken()._2 should equal(expectedTokens(i))
    }
  }

  property("Should return the same result from example (c)") {
    val expectedTokens: List[Chat.Token] = List(
      Token.BONJOUR,
      Token.JE,
      Token.ETRE,
      Token.PSEUDO,
      Token.EOL
    )

    val input = "Bonjour, je suis _Michel !"
    val tokens = tokenizerSvc.tokenize(input)

    // Loop over the tokens and check if they are the same as the expected tokens
    for (i <- 0 until expectedTokens.length) {
      tokens.nextToken()._2 should equal(expectedTokens(i))
    }
  }

  property("Should return the same result from example (d)") {
    val expectedTokens: List[Chat.Token] = List(
      Token.BONJOUR,
      Token.JE,
      Token.VOULOIR,
      Token.NUM,
      Token.PRODUIT,
      Token.ET,
      Token.NUM,
      Token.PRODUIT,
      Token.SVP,
      Token.EOL
    )

    val input = "bonuour ja veut 8 bbères et 5 crsoiasabsdts stppp..."
    val tokens = tokenizerSvc.tokenize(input)

    // Loop over the tokens and check if they are the same as the expected tokens
    for (i <- 0 until expectedTokens.length) {
      tokens.nextToken()._2 should equal(expectedTokens(i))
    }
  }
}
