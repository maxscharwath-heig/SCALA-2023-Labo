import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import Chat.{AnalyzerService, TokenizerService}
import Utils.{Dictionary, SpellCheckerService, SpellCheckerImpl}
import Data.{AccountImpl, ProductImpl, SessionImpl}
import Data.ProductService
import Data.AccountService
import Data.SessionService
import Chat.Parser

class AnalyzerServiceSuite
    extends AnyWordSpec
    with Matchers {
  val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(Dictionary.dictionary)
  val tokenizerSvc: TokenizerService = new TokenizerService(spellCheckerSvc)
  val productSvc: ProductService = new ProductImpl()
  val accountSvc: AccountService = new AccountImpl()
  val analyzerSvc: AnalyzerService = new AnalyzerService(productSvc, accountSvc)
  val sessionSvc: SessionService = new SessionImpl()
  val session = sessionSvc.create()

  "Test conversation" should {
    "handle Thirsty input" in {
      val thirstyInput = "Bonjour, je suis assoiffé !"
      val thirstyTokens = tokenizerSvc.tokenize(thirstyInput)
      val thirstyParser = new Parser(thirstyTokens)
      val thirstyExpr = thirstyParser.parsePhrases()
      val thirstyResult = analyzerSvc.reply(session)(thirstyExpr)
      thirstyResult._1 should include("Eh bien, la chance est de votre côté car nous offrons les meilleures bières de la région !")
    }

    "handle Auth input" in {
      val authInput = "Je suis _Michel."
      val authTokens = tokenizerSvc.tokenize(authInput)
      val authParser = new Parser(authTokens)
      val authExpr = authParser.parsePhrases()
      val authResult = analyzerSvc.reply(session)(authExpr)
      authResult._1 should include("Bonjour, michel !")
    }

    "handle Price input" in {
      val priceInput = "Combien coûte 1 bière PunkIPA ?"
      val priceTokens = tokenizerSvc.tokenize(priceInput)
      val priceParser = new Parser(priceTokens)
      val priceExpr = priceParser.parsePhrases()
      val priceResult = analyzerSvc.reply(session)(priceExpr)
      priceResult._1 should include("Cela coûte CHF 3.0.")
    }

    "handle Solde input" in {
      val soldeInput = "J'aimerais connaitre mon solde."
      val soldeTokens = tokenizerSvc.tokenize(soldeInput)
      val soldeParser = new Parser(soldeTokens)
      val soldeExpr = soldeParser.parsePhrases()
      val soldeResult = analyzerSvc.reply(session)(soldeExpr)
      soldeResult._1 should include("solde est de CHF 30.0.")
    }

    "handle another Auth input" in {
      val authInput = "Bonjour ! Je suis _Bobby."
      val authTokens = tokenizerSvc.tokenize(authInput)
      val authParser = new Parser(authTokens)
      val authExpr = authParser.parsePhrases()
      val authResult = analyzerSvc.reply(session)(authExpr)
      authResult._1 should include("Bonjour, bobby !")
    }

    "handle Hungry input" in {
      val hungryInput = "Je suis affamé !"
      val hungryTokens = tokenizerSvc.tokenize(hungryInput)
      val hungryParser = new Parser(hungryTokens)
      val hungryExpr = hungryParser.parsePhrases()
      val hungryResult = analyzerSvc.reply(session)(hungryExpr)
      hungryResult._1 should include("Pas de soucis ! Nous pouvons vous offrir des croissants faits maisons !")
    }

    "handle price with associability" in {
      val priceInput = "Combien coute 1 biere punkipa et 1 biere boxer ou 1 biere farmer ou 1 biere tenebreuse et 1 biere boxer"
      val priceTokens = tokenizerSvc.tokenize(priceInput)
      val priceParser = new Parser(priceTokens)
      val priceExpr = priceParser.parsePhrases()
      val priceResult = analyzerSvc.reply(session)(priceExpr)
      priceResult._1 should include("Cela coûte CHF 2.0.")
    }
  }
}
