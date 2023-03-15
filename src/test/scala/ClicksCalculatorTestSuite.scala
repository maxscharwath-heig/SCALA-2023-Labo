import org.scalatest.*
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import prop.*
import Utils.{ClinksCalculator}

class ClicksCalculatorTestSuite
    extends AnyPropSpec
    with TableDrivenPropertyChecks
    with should.Matchers {
  val factorial = ClinksCalculator.factorial

  // Factorials tests
  property("Factorial of 0 should be 1") {
    factorial(0) should equal(1)
  }

  property("Factorial of 1 should be 1") {
    factorial(1) should equal(1)
  }

  property("Factorial of 2 should be 2") {
    factorial(2) should equal(2)
  }

  property("Factorial of 12 should be 479001600") {
    factorial(12) should equal(479001600)
  }

  property("Factorial of -1 should throw an exception") {
    assertThrows[IllegalArgumentException] {
      factorial(-1)
    }
  }

  // Clinks tests
  property("Combination of 0 and 0 should be 1") {
    ClinksCalculator.calculateCombination(0, 0) should equal(1)
  }

  property("Combination of 1 and 1 should be 1") {
    ClinksCalculator.calculateCombination(1, 1) should equal(1)
  }

  property("Combination of 3 and 12 should be 220") {
    ClinksCalculator.calculateCombination(12, 3) should equal(220)
  }

  property("Combination of 2 and 1 should throw an exception because k > n") {
    assertThrows[IllegalArgumentException] {
      ClinksCalculator.calculateCombination(1, 2)
    }
  }

  property("Combination of -1 and 1 should throw an exception") {
    assertThrows[IllegalArgumentException] {
      ClinksCalculator.calculateCombination(-1, 1)
    }
  }

  property("Combination of 1 and -1 should throw an exception") {
    assertThrows[IllegalArgumentException] {
      ClinksCalculator.calculateCombination(1, -1)
    }
  }

}
