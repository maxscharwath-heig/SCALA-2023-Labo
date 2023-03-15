package Utils

/** Contains the function necessary to calculate the number of *clinks* when n
  * people want to cheers.
  */
object ClinksCalculator:
  /** Calculate the factorial of a given number
    * @param n
    *   the number to compute
    * @return
    *   n!
    * @throws IllegalArgumentException
    *  if n < 0
    */
  def factorial(n: Int): BigInt = {
    if(n < 0) throw new IllegalArgumentException("n must be greater than 0")
    // Usage of tail recursion
    def fact(n: Int, acc: BigInt): BigInt = {
      if (n == 0) then acc else fact(n - 1, n * acc)
    }
    fact(n, 1)
  }

  /** Calculate the combination of two given numbers
    * @param n
    *   the first number
    * @param k
    *   the second number
    * @return
    *   n choose k
    * @throws IllegalArgumentException
    * if k > n or if n < 0 or if k < 0
    */
  def calculateCombination(n: Int, k: Int): BigInt = {
    if (k > n) {
      throw new IllegalArgumentException("k must be less than or equal to n")
    }
    if (n < 0 || k < 0) {
      throw new IllegalArgumentException("n and k must be greater than 0")
    }
    return (factorial(n) / (factorial(k) * factorial(n - k)))
  }
end ClinksCalculator
