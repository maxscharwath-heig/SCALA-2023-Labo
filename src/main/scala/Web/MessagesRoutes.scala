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
  def postMessage(msg: String)(session: Session) = {
    if (session.getCurrentUser.isEmpty) {
      ujson.Obj(
        "success" -> false,
        "err" -> "You must be logged in to send messages"
      )
    } else if (msg.isEmpty) {
      ujson.Obj("success" -> false, "err" -> "A message cannot be empty")
    } else {

      val jsonResponse = processMessage(msg)(session)

      val latests = latestMessagesAsString(20)
      subscribers.foreach(_.send(cask.Ws.Text(latests)))

      jsonResponse
    }
  }

  @cask.websocket("/subscribe")
  def subscribe() = {
    cask.WsHandler { chan =>
      subscribers += chan

      val latests = latestMessagesAsString(20)
      subscribers.foreach(_.send(cask.Ws.Text(latests)))

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

  private def latestMessagesAsString(number: Int) = {
    val messages = msgSvc.getLatestMessages(number)

    messages match {
      case Nil => Layouts.placeholderContent("No message yet").toString
      case _ =>
        messages.reverse
          .map((user, msg) => Layouts.message(user, msg).toString)
          .mkString
    }
  }

  private def processMessage(msg: String)(session: Session) = {
    // Parse the mention of me the message
    val (mentionnedUser, content) = parseMessageMention(msg)

    mentionnedUser match {
      case Some("@bot") => {
        // Get the bot reply
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
          expr match {
            case Auth(username) => {
              msgSvc.add(
                "bot",
                Layouts.messageContent("Hello " + username, None),
                session.getCurrentUser,
                None,
                Some(userMsgId)
              )
              ujson.Obj("success" -> true, "err" -> "")
            }

            case _ => {
              // Get the reply of the bot
              val reply = analyzerSvc.reply(session)(expr)
              msgSvc.add(
                "bot",
                Layouts.messageContent(reply, None),
                session.getCurrentUser,
                None,
                Some(userMsgId)
              )
              ujson.Obj("success" -> true, "err" -> "")
            }
          }

        } catch {
          case e: Exception =>
            ujson.Obj("success" -> false, "err" -> e.getMessage())
        }
      }

      case Some(anyUser) => {
        msgSvc.add(
          session.getCurrentUser.get,
          Layouts.messageContent(content, Some(anyUser))
        )
        ujson.Obj("success" -> true, "err" -> "")
      }

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
