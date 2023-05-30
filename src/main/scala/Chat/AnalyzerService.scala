// SCALA - Labo 2
// Nicolas Crausaz & Maxime Scharwath

package Chat

import Data.{AccountService, ProductService, Session}
import ExprTree._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):

  // Default solde for new users
  private val DEFAULT_SOLDE = 30.0

  // Helper method to compute the price of a product
  private def computeProductPrice(
      name: String,
      brand: String,
      quantity: Int
  ): Double =
    quantity * productSvc.getPrice(name, brand)

  // Helper method to handle authentication
  private def handleAuth(session: Session, name: String): String = {
    if (!accountSvc.isAccountExisting(name))
      accountSvc.addAccount(name, DEFAULT_SOLDE)

    session.setCurrentUser(name)
    s"Bonjour, $name !"
  }

  // Helper method to process a command
  private def processCommand(
      session: Session,
      products: ExprTree,
      inner: ExprTree => (String, Option[Future[String]])
  ): (String, Option[Future[String]]) =
    session.getCurrentUser match {
      case Some(user) => {
        val finalPrice = computePrice(products)

        if (finalPrice > accountSvc.getAccountBalance(user))
          (
            "Vous n'avez pas assez d'argent pour effectuer cette commande.",
            None
          )
        else {
          val futureAnswer = Future {
            accountSvc.purchase(user, finalPrice)

            // s"Voici donc ${inner(products)._1} ! Cela coûte CHF $finalPrice et votre nouveau solde est de CHF ${accountSvc
            //     .getAccountBalance(user)}."

            s"La commande de ${inner(products)._1} est prête. Cela coûte $finalPrice.-"

            // TODO: ajouter le cas de la commande partielle

          }.recover(_ =>
            s"La commande de ${inner(products)._1} ne peut pas être délivrée"
          )

          (
            s"Votre commande est en cours de préparation : ${inner(products)._1}",
            Some(futureAnswer)
          )

        }
      }
      case None => ("Veuillez d'abord vous identifier.", None)
    }

  // Helper method to get the account balance
  private def getAccountBalance(session: Session): String =
    session.getCurrentUser match {
      case Some(user) =>
        s"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(user)}."
      case None => "Veuillez d'abord vous identifier."
    }

  /** Compute the price of the current node, then returns it. If the node is not
    * a computational node, the method returns 0.0.
    * @return
    *   the result of the computation
    */
  def computePrice(t: ExprTree): Double = t match {
    case Product(name, brand, quantity) =>
      computeProductPrice(name, brand, quantity)
    case Command(products) => computePrice(products)
    case Or(left, right)   => Math.min(computePrice(left), computePrice(right))
    case And(left, right)  => computePrice(left) + computePrice(right)
    case _                 => 0.0
  }

  /** Return the output text of the current node, in order to write it in
    * console.
    * @return
    *   the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): (String, Option[Future[String]]) = {
    val inner: ExprTree => (String, Option[Future[String]]) = reply(session)
    t match {
      case Thirsty =>
        (
          "Eh bien, la chance est de votre côté car nous offrons les meilleures bières de la région !",
          None
        )
      case Hungry =>
        (
          "Pas de soucis ! Nous pouvons vous offrir des croissants faits maisons !",
          None
        )
      case Price(expr) => (s"Cela coûte CHF ${computePrice(expr)}.", None)
      case Product(name, brand, quantity) =>
        (
          s"$quantity $name ${
              if (brand.isEmpty()) productSvc.getDefaultBrand(name) else brand
            }",
          None
        )
      case Auth(name)       => (handleAuth(session, name), None)
      case Solde            => (getAccountBalance(session), None)
      case And(left, right) => (s"${inner(left)} et ${inner(right)}", None)
      case Or(left, right) =>
        if (computePrice(left) < computePrice(right)) inner(left)
        else inner(right)
      case Command(products) => {
        processCommand(session, products, inner)
      }
    }
  }

end AnalyzerService
