package Web

import Chat.{AnalyzerService, TokenizerService}
import Data.{MessageService, AccountService, SessionService, Session}
import scala.collection.mutable.ListBuffer
import castor.Context.Simple.global
import cask.endpoints.WsChannelActor
import Chat.Parser
import Chat.ExprTree.Auth
import scala.quoted.ToExpr.NoneToExpr

/** Assembles the routes dealing with the message board:
  *   - One route to display the home page
  *   - One route to send the new messages as JSON
  *   - One route to subscribe with websocket to new messages
  *
  * @param log
  */
class MessagesRoutes(
    tokenizerSvc: TokenizerService,
    analyzerSvc: AnalyzerService,
    msgSvc: MessageService,
    accountSvc: AccountService,
    sessionSvc: SessionService
)(implicit val log: cask.Logger)
    extends cask.Routes:
  import Decorators.getSession

  val subscribers = ListBuffer[WsChannelActor]()

  @getSession(sessionSvc)
  @cask.get("/")
  def index()(session: Session) = Layouts.index(session)

  @getSession(sessionSvc)
  @cask.postJson("/send")
  def postMessage(msg: String)(session: Session): ujson.Obj = {
    if (session.getCurrentUser.isEmpty) {
      return ujson.Obj(
        "success" -> false,
        "err" -> "You must be logged in to send messages"
      )
    }

    if (msg.isEmpty) {
      return ujson.Obj("success" -> false, "err" -> "A message cannot be empty")
    }

    val jsonResponse = processMessage(msg)(session)

    val latests = latestMessagesAsCaskWSString(20)
    subscribers.foreach(_.send(latests))

    jsonResponse
  }

  @cask.websocket("/subscribe")
  def subscribe() = {
    cask.WsHandler { chan =>
      subscribers += chan

      val latests = latestMessagesAsCaskWSString(20)
      subscribers.foreach(_.send(latests))

      cask.WsActor { case cask.Ws.Close(_, _) =>
        subscribers -= chan
      }
    }
  }

  @cask.get("/clearHistory")
  def clearHistory() = {
    msgSvc.deleteHistory()
    cask.Redirect("/")
  }

  private def latestMessagesAsCaskWSString(number: Int): cask.Ws.Text = {
    val messages = msgSvc.getLatestMessages(number)

    val text = messages match {
      case Nil => Layouts.placeholderContent("No message where received yet").toString
      case _ =>
        messages
          .map((user, msg) => Layouts.message(user, msg).toString)
          .mkString
    }

    cask.Ws.Text(text)
  }

  private def processMessage(msg: String)(session: Session) = {
    // Parse the mention of me the message
    val (mentionnedUser, content) = parseMessageMention(msg)

    mentionnedUser match {
      // Mention of bot
      case Some("@bot") => {
        val tokens = tokenizerSvc.tokenize(content)

        try {
          val expr = new Parser(tokens).parsePhrases()
          // Add the message of user
          val userMsgId = msgSvc.add(
            session.getCurrentUser.get,
            Layouts.messageContent(content, mentionnedUser),
            mentionnedUser,
            Some(expr)
          )

          // Identification message must not be analyzed to prevent changes of session user
          val messageContent = expr match {
            case Auth(username) =>
              Layouts.messageContent("Hello " + username, None)
            case _ => {
              // Get the reply of the bot
              Layouts.messageContent(analyzerSvc.reply(session)(expr), None)
            }
          }

          // Send the reply of the bot
          msgSvc.add(
            "bot",
            messageContent,
            session.getCurrentUser,
            None,
            Some(userMsgId)
          )
          ujson.Obj("success" -> true, "err" -> "")

        } catch {
          case e: Exception =>
            ujson.Obj("success" -> false, "err" -> e.getMessage())
        }
      }

      // Mention of any user
      case Some(anyUser) => {
        msgSvc.add(
          session.getCurrentUser.get,
          Layouts.messageContent(content, Some(anyUser))
        )
        ujson.Obj("success" -> true, "err" -> "")
      }

      // No mention
      case None => {
        msgSvc.add(
          session.getCurrentUser.get,
          Layouts.messageContent(content, None)
        )
        ujson.Obj("success" -> true, "err" -> "")
      }
    }
  }

  /** Split the mention of a user in a message and the content of the message
    * @param msg
    *   the message
    * @return
    *   a tuple with the mentionned user (if any) and the content of the message
    */
  private def parseMessageMention(msg: String): (Option[String], String) = {
    msg(0) match {
      case '@' =>
        val (mentionnedUser, content) = msg.splitAt(msg.indexOf(' '))
        (Some(mentionnedUser), content)
      case _ => (None, msg)
    }
  }

  initialize()
end MessagesRoutes
