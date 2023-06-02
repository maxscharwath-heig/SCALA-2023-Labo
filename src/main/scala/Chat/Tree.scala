// SCALA - Labo 4
// Nicolas Crausaz & Maxime Scharwath

package Chat

/** This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/** Declarations of the nodes' types.
  */
object ExprTree:
  // Etats
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree

  // Actions
  case class Auth(username: String) extends ExprTree
  case class Price(products: ExprTree) extends ExprTree
  case class Command(products: ExprTree) extends ExprTree
  case object Solde extends ExprTree

  // Products
  case class Product(name: String, brand: Option[String], quantity: Int)
      extends ExprTree

  // Operators
  case class Or(left: ExprTree, right: ExprTree) extends ExprTree
  case class And(left: ExprTree, right: ExprTree) extends ExprTree
