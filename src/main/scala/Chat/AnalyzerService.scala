package Chat
import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import ExprTree._

  /** Default "solde" for new users
    */
  private val DEFAULT_SOLDE = 100.0

  /** Compute the price of the current node, then returns it. If the node is not
    * a computational node, the method returns 0.0. For example if we had a "+"
    * node, we would add the values of its two children, then return the result.
    * @return
    *   the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double = {
    t match
      case Product(name, brand, quantity) =>
        quantity * productSvc.getPrice(name, brand)
      case Command(products) => computePrice(products)
      case Or(left, right)  => Math.min(computePrice(left), computePrice(right))
      case And(left, right) => computePrice(left) + computePrice(right)
      case _                => 0.0
  }

  /** Return the output text of the current node, in order to write it in
    * console.
    * @return
    *   the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    t match
      // TODO - Part 2 Step 3
      case Thirsty =>
        "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"

      case Hungry =>
        "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"

      case Price => s"Cela coûte CHF ${computePrice(t)}"

      case Auth(name) => {
        // If user does not exist, create it and add default solde
        if (!accountSvc.isAccountExisting(name))
          accountSvc.addAccount(name, DEFAULT_SOLDE)

        session.setCurrentUser(name)
        s"Bonjour $name, !"
      }

      case Solde => {
        session.getCurrentUser match
          case Some(user) =>
            s"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(user)}"
          case None => "Veuillez d'abord vous identifier."
      }

      case Command(products) => {
        session.getCurrentUser match
          case Some(user) => {
            val finalPrice = computePrice(products)

            if (finalPrice > accountSvc.getAccountBalance(user))
              "Vous n'avez pas assez d'argent pour effectuer cette commande."
            else {
              accountSvc.purchase(user, finalPrice)
              "Votre commande a bien été prise en compte."
            }
          }
          case None => "Veuillez d'abord vous identifier."
      }

      case _ => "Je ne comprends pas votre demande."
end AnalyzerService
