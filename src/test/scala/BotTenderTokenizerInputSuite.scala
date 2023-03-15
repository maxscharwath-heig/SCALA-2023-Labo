import org.scalatest.*
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import prop.*

import java.io.ByteArrayOutputStream
import Utils.{Dictionary, SpellCheckerService, SpellCheckerImpl}
import Chat.TokenizerService

class BotTenderTokenizerInputSuite extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers {
    val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(Dictionary.dictionary)
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
        outCapture.toString() should include ("Adieu.")
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
}
