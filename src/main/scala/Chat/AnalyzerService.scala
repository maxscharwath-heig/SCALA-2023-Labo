// SCALA - Labo 4
// Nicolas Crausaz & Maxime Scharwath

// TODO: Par exemple : Si on commande 2 croissants maison et 1 croissant caillier, la préparation
// du croissant caillier et du premier croissant maison commence en même temps alors que la
// préparation du deuxième croissant maison commence quand le premier croissant maison est
// prêt.
// -> TODO: Prendre en compte le nombre de produits commandés

package Chat

import Data.{AccountService, ProductService, Session}
import ExprTree._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):

  // Helper enum to handle the three cases of preparation status
  private enum PreparationStatus:
    case Success, Partial, Failure

  // Default solde for new users
  private val DEFAULT_SOLDE = 30.0

  // Helper method to compute the price of a product
  private def computeProductPrice(
      name: String,
      brand: Option[String],
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

  private def prepare(
      products: ExprTree
  ): (Future[(Option[ExprTree], PreparationStatus)]) = {
    products match
      case Product(name, brand, quantity) => {
        val preparation = productSvc.startPreparation(name, brand)
        preparation
          .map { _ =>
            (Some(Product(name, brand, quantity)), PreparationStatus.Success)
          }
          .recover(_ => (None, PreparationStatus.Failure))
      }

      case And(left, right) => {
        val tasks = Future.sequence(Seq(prepare(left), prepare(right)))
        tasks.map { results =>
          // All success
          if results.forall(_._2 == PreparationStatus.Success) then
            (Some(And(left, right)), PreparationStatus.Success)

          // All failed
          else if results.forall(_._2 == PreparationStatus.Failure) then
            (None, PreparationStatus.Failure)

          // One failed, return the one that didn't
          else
            (
              results.filter(_._2 != PreparationStatus.Failure).head._1,
              PreparationStatus.Partial
            )
        }
      }

      case Or(left, right) =>
        if computePrice(left) <= computePrice(right) then prepare(left)
        else prepare(right)

      // Nothing to prepare
      case _ => Future.successful((Some(products), PreparationStatus.Success))
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
          s"$quantity $name ${brand.getOrElse(productSvc.getDefaultBrand(name))}",
          None
        )
      case Auth(name) => (handleAuth(session, name), None)
      case Solde      => (getAccountBalance(session), None)
      case And(left, right) =>
        (s"${inner(left)._1} et ${inner(right)._1}", None)
      case Or(left, right) =>
        if (computePrice(left) < computePrice(right)) inner(left)
        else inner(right)
      case Command(products) => {
        session.getCurrentUser match {
          case Some(user) => {
            val orderPrice = computePrice(products)
            if (orderPrice > accountSvc.getAccountBalance(user)) {
              // TODO: check the error on the client side when this case appears
              (
                "Vous n'avez pas assez d'argent pour effectuer cette commande.",
                None
              )
            } else {
              val baseOrder = inner(products)._1
              val order = prepare(products)
                .map(task => {
                  task._2 match
                    case PreparationStatus.Success => {
                      accountSvc.purchase(user, orderPrice)
                      s"La commande de ${baseOrder} est prête. Cela coute $orderPrice.-"
                    }

                    case PreparationStatus.Partial => {
                      val ajustedPrice = computePrice(task._1.get)
                      val adjustedOrder = inner(task._1.get)._1
                      accountSvc.purchase(user, ajustedPrice)
                      s"La commande de ${baseOrder} est partiellement prête. Voici ${adjustedOrder}. Cela coute $ajustedPrice.-"
                    }

                    case PreparationStatus.Failure => {
                      s"La commande de ${baseOrder} ne peut pas être délivrée."
                    }
                })
              (
                s"Votre commande est en cours de préparation: ${baseOrder}",
                Some(order)
              )
            }
          }
          case None => ("Veuillez d'abord vous identifier.", None)
        }
      }
    }
  }

end AnalyzerService
