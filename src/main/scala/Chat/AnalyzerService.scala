// SCALA - Labo 4
// Nicolas Crausaz & Maxime Scharwath

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
        // Running the preparation sequentially
        val preparations = (1 to quantity).foldLeft(
          Future.successful(List.empty[PreparationStatus])
        ) { (previousFuture, _) =>
          previousFuture.flatMap { list =>
            productSvc
              .startPreparation(name, brand)
              .map(_ => PreparationStatus.Success)
              .recover(_ => PreparationStatus.Failure)
              .map(result => list :+ result)
          }
        }

        preparations.flatMap(results => {
          // All success
          if results.forall(_ == PreparationStatus.Success) then
            Future.successful(
              (Some(Product(name, brand, quantity)), PreparationStatus.Success)
            )

          // All failed
          else if results.forall(_ == PreparationStatus.Failure) then
            Future.successful((None, PreparationStatus.Failure))

          // Some failed
          else
            Future.successful(
              (
                Some(
                  Product(
                    name,
                    brand,
                    results.count(_ == PreparationStatus.Success)
                  )
                ),
                PreparationStatus.Partial
              )
            )
        })
      }

      case And(left, right) => {
        // Running the preparations in parallel
        val leftPreparation = prepare(left)
        val rightPreparation = prepare(right)

        for {
          (leftResult, leftStatus) <- leftPreparation
          (rightResult, rightStatus) <- rightPreparation
        } yield {
          (leftStatus, rightStatus) match {
            // Both success
            case (PreparationStatus.Success, PreparationStatus.Success) =>
              (Some(And(left, right)), PreparationStatus.Success)

            // Both failed
            case (PreparationStatus.Failure, PreparationStatus.Failure) =>
              (None, PreparationStatus.Failure)

            // Both partial
            case (PreparationStatus.Partial, PreparationStatus.Partial) =>
              (
                Some(And(leftResult.get, rightResult.get)),
                PreparationStatus.Partial
              )

            // One failed, return the one that didn't
            case _ =>
              (leftResult.orElse(rightResult), PreparationStatus.Partial)
          }
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
              throw new Exception(
                "Vous n'avez pas assez d'argent pour effectuer cette commande."
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
