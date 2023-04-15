package Chat

import Data.{AccountService, ProductService, Session}
import ExprTree._

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):

  // Default solde for new users
  private val DEFAULT_SOLDE = 30.0

  // Helper method to compute the price of a product
  private def computeProductPrice(name: String, brand: String, quantity: Int): Double =
    quantity * productSvc.getPrice(name, brand)

  // Helper method to handle authentication
  private def handleAuth(session: Session, name: String): String = {
    if (!accountSvc.isAccountExisting(name))
      accountSvc.addAccount(name, DEFAULT_SOLDE)

    session.setCurrentUser(name)
    s"Bonjour $name !"
  }

  // Helper method to process a command
  private def processCommand(session: Session, products: ExprTree, inner: ExprTree => String): String =
    session.getCurrentUser match {
      case Some(user) => {
        val finalPrice = computePrice(products)

        if (finalPrice > accountSvc.getAccountBalance(user))
          "Vous n'avez pas assez d'argent pour effectuer cette commande."
        else {
          accountSvc.purchase(user, finalPrice)
          s"Voici donc ${inner(products)} ! Cela coûte $finalPrice et votre nouveau solde est de ${accountSvc.getAccountBalance(user)}"
        }
      }
      case None => "Veuillez d'abord vous identifier."
    }

  // Helper method to get the account balance
  private def getAccountBalance(session: Session): String =
    session.getCurrentUser match {
      case Some(user) =>
        s"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(user)}"
      case None => "Veuillez d'abord vous identifier."
    }

  /** Compute the price of the current node, then returns it. If the node is not
    * a computational node, the method returns 0.0.
    * @return
    *   the result of the computation
    */
  def computePrice(t: ExprTree): Double = t match {
    case Product(name, brand, quantity) => computeProductPrice(name, brand, quantity)
    case Command(products)              => computePrice(products)
    case Or(left, right)                => Math.min(computePrice(left), computePrice(right))
    case And(left, right)               => computePrice(left) + computePrice(right)
    case _                              => 0.0
  }

  /** Return the output text of the current node, in order to write it in
    * console.
    * @return
    *   the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String = {
    val inner: ExprTree => String = reply(session)
    t match {
      case Thirsty => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry  => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case Price(expr) => s"Cela coûte CHF ${computePrice(expr)}"
      case Product(name, brand, quantity) =>
        s"$quantity $name ${if (brand.isEmpty()) productSvc.getDefaultBrand(name) else brand}"
      case Auth(name) => handleAuth(session, name)
      case Solde => getAccountBalance(session)
      case Command(products) => processCommand(session, products, inner)
      case And(left, right) => s"${inner(left)} et ${inner(right)}"
      case Or(left, right)  => if (computePrice(left) < computePrice(right)) inner(left) else inner(right)
    }
  }

end AnalyzerService