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
      thirstyResult should include("Eh bien, la chance est de votre côté car nous offrons les meilleures bières de la région !")
    }

    "handle Auth input" in {
      val authInput = "Je suis _Michel."
      val authTokens = tokenizerSvc.tokenize(authInput)
      val authParser = new Parser(authTokens)
      val authExpr = authParser.parsePhrases()
      val authResult = analyzerSvc.reply(session)(authExpr)
      authResult should include("Bonjour, michel !")
    }

    "handle Price input" in {
      val priceInput = "Combien coûte 1 bière PunkIPA ?"
      val priceTokens = tokenizerSvc.tokenize(priceInput)
      val priceParser = new Parser(priceTokens)
      val priceExpr = priceParser.parsePhrases()
      val priceResult = analyzerSvc.reply(session)(priceExpr)
      priceResult should include("Cela coûte CHF 3.0.")
    }

    "handle Solde input" in {
      val soldeInput = "J'aimerais connaitre mon solde."
      val soldeTokens = tokenizerSvc.tokenize(soldeInput)
      val soldeParser = new Parser(soldeTokens)
      val soldeExpr = soldeParser.parsePhrases()
      val soldeResult = analyzerSvc.reply(session)(soldeExpr)
      soldeResult should include("solde est de CHF 30.0.")
    }

    "handle Command Biers input" in {
      val commandInput = "Je veux commander 2 bières PunkIPAs et 1 bière Ténébreuse."
      val commandTokens = tokenizerSvc.tokenize(commandInput)
      val commandParser = new Parser(commandTokens)
      val commandExpr = commandParser.parsePhrases()
      val commandResult = analyzerSvc.reply(session)(commandExpr)
      commandResult should include("Voici donc 2 biere punkipa et 1 biere tenebreuse !")
      commandResult should include("Cela coûte CHF 10.0")
      commandResult should include("solde est de CHF 20.0")
    }

    "handle Command Croissant input" in {
      val commandInput = "Je voudrais commander 1 croissant."
      val commandTokens = tokenizerSvc.tokenize(commandInput)
      val commandParser = new Parser(commandTokens)
      val commandExpr = commandParser.parsePhrases()
      val commandResult = analyzerSvc.reply(session)(commandExpr)
      commandResult should include("Voici donc 1 croissant maison !")
      commandResult should include("Cela coûte CHF 2.0")
      commandResult should include("solde est de CHF 18.0")
    }

    "handle another Auth input" in {
      val authInput = "Bonjour ! Je suis _Bobby."
      val authTokens = tokenizerSvc.tokenize(authInput)
      val authParser = new Parser(authTokens)
      val authExpr = authParser.parsePhrases()
      val authResult = analyzerSvc.reply(session)(authExpr)
      authResult should include("Bonjour, bobby !")
    }

    "handle Hungry input" in {
      val hungryInput = "Je suis affamé !"
      val hungryTokens = tokenizerSvc.tokenize(hungryInput)
      val hungryParser = new Parser(hungryTokens)
      val hungryExpr = hungryParser.parsePhrases()
      val hungryResult = analyzerSvc.reply(session)(hungryExpr)
      hungryResult should include("Pas de soucis ! Nous pouvons vous offrir des croissants faits maisons !")
    }

    "handle another Command input" in {
      val commandInput = "Je veux commander 2 croissants cailler."
      val commandTokens = tokenizerSvc.tokenize(commandInput)
      val commandParser = new Parser(commandTokens)
      val commandExpr = commandParser.parsePhrases()
      val commandResult = analyzerSvc.reply(session)(commandExpr)
      commandResult should include("Voici donc 2 croissant cailler !")
      commandResult should include("Cela coûte CHF 4.0")
      commandResult should include("solde est de CHF 26.0")
    }

    "handle another Solde input" in {
      val soldeInput = "J'aimerais connaitre mon solde."
      val soldeTokens = tokenizerSvc.tokenize(soldeInput)
      val soldeParser = new Parser(soldeTokens)
      val soldeExpr = soldeParser.parsePhrases()
      val soldeResult = analyzerSvc.reply(session)(soldeExpr)
      soldeResult should include("solde est de CHF 26.0.")
    }

    "switch again to Michel and command 18 Farmers" in {
      // Switch to Michel
      analyzerSvc.reply(session)(new Parser(tokenizerSvc.tokenize("Je suis _Michel.")).parsePhrases())

      val commandInput = "J'aimerais commander 18 bières Farmer."
      val commandTokens = tokenizerSvc.tokenize(commandInput)
      val commandParser = new Parser(commandTokens)
      val commandExpr = commandParser.parsePhrases()
      val commandResult = analyzerSvc.reply(session)(commandExpr)
      commandResult should include("Voici donc 18 biere farmer !")
      commandResult should include("Cela coûte CHF 18.0")
      commandResult should include("solde est de CHF 0.0")
    }

    "handle command with no money" in {
      val commandInput = "J'aimerais commander 1 bière Farmer."
      val commandTokens = tokenizerSvc.tokenize(commandInput)
      val commandParser = new Parser(commandTokens)
      val commandExpr = commandParser.parsePhrases()
      val commandResult = analyzerSvc.reply(session)(commandExpr)
      commandResult should include("Vous n'avez pas assez d'argent pour effectuer cette commande.")
    }

    "handle price with associability" in {
      val priceInput = "Combien coute 1 biere punkipa et 1 biere boxer ou 1 biere farmer ou 1 biere tenebreuse et 1 biere boxer"
      val priceTokens = tokenizerSvc.tokenize(priceInput)
      val priceParser = new Parser(priceTokens)
      val priceExpr = priceParser.parsePhrases()
      val priceResult = analyzerSvc.reply(session)(priceExpr)
      priceResult should include("Cela coûte CHF 2.0.")
    }
  }
}
