package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Example cases
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree
  case class Auth(username: String) extends ExprTree
  case object Solde extends ExprTree
  case object Price extends ExprTree // Ask for the price of a product
