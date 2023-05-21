package Data

import Chat.ExprTree
import Data.MessageService.{MsgContent, Username}
import scalatags.Text.Frag

object MessageService:
  type Username = String
  type MsgContent = Frag

trait MessageService:
  /** Retrieve the latest N added messages
    * @param n
    *   The number of message to retrieve
    * @return
    *   The content of the messages and their senders
    */
  def getLatestMessages(n: Int): Seq[(Username, MsgContent)]

  /** Add a message to the history
    * @param sender
    *   The username of the sender
    * @param msg
    *   The content of the message
    * @param mention
    *   The name if it exists of the user mentioned at the start of the message
    *   with an '@'. For example the message "@Julian Hello" mentions "Julian"
    * @param exprType
    *   If the message is to the bot, the type of the query
    * @param replyToId
    *   If the message is a reply to another message, the id of the other
    *   message. Used for example, when the bot answers to a query
    * @return
    *   the unique id of the added message (generated by a method of your
    *   choice)
    */
  def add(
      sender: Username,
      msg: MsgContent,
      mention: Option[Username] = None,
      exprType: Option[ExprTree] = None,
      replyToId: Option[Long] = None
  ): Long

  /** Deletes all the stored messages
    */
  def deleteHistory(): Unit

end MessageService

case class Message(
    sender: Username,
    msg: MsgContent,
    mention: Option[Username],
    exprType: Option[ExprTree],
    replyToId: Option[Long]
)

class MessageImpl extends MessageService:

  private var messages: List[Message] = Nil

  private var id: Long = 1

  override def add(
      sender: Username,
      msg: MsgContent,
      mention: Option[Username] = None,
      exprType: Option[ExprTree] = None,
      replyToId: Option[Long] = None
  ): Long = {
    messages = messages :+ Message(sender, msg, mention, exprType, replyToId)
    id += 1
    id
  }

  override def getLatestMessages(n: Int) =
    messages.takeRight(n).map(m => (m.sender, m.msg))

  override def deleteHistory(): Unit = messages = Nil
