package Web

import Chat.ExprTree.Auth
import Chat.{AnalyzerService, ExprTree, Parser, TokenizerService}
import Data.{AccountService, MessageService, Session, SessionService}
import cask.endpoints.{WsChannelActor, WsHandler}
import cask.model.Response
import castor.Context.Simple.global
import scalatags.Text

import scala.collection.mutable.ListBuffer
import scala.quoted.ToExpr.NoneToExpr

/** Assembles the routes dealing with the message board:
 *   - One route to display the home page
 *   - One route to send the new messages as JSON
 *   - One route to subscribe with websocket to new messages
 *
 * @param tokenizerSvc the tokenizer service
 * @param analyzerSvc the analyzer service
 * @param msgSvc the message service
 * @param accountSvc the account service
 * @param sessionSvc the session service
 * @param log the logger
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

  private val subscribers = ListBuffer[WsChannelActor]()

  @getSession(sessionSvc)
  @cask.get("/")
  def index()(session: Session): Text.TypedTag[String] = Layouts.index(session)

  @getSession(sessionSvc)
  @cask.postJson("/send")
  def postMessage(msg: String)(session: Session): ujson.Obj = {
    if (session.getCurrentUser.isEmpty)
      jsonResponse(
        success = false,
        err = "You must be logged in to send messages"
      )
    else if (msg.isEmpty)
      jsonResponse(success = false, err = "A message cannot be empty")
    else
      val response = processMessage(msg)(session)
      notifySubscribers(20)
      response
  }

  private def notifySubscribers(number: Int): Unit = {
    subscribers.foreach(
      _.send(latestMessagesAsCaskWSString(number))
    )
  }

  private def latestMessagesAsCaskWSString(number: Int): cask.Ws.Text = {
    val messages = msgSvc.getLatestMessages(number)

    val text = messages match {
      case Nil =>
        Layouts.placeholderContent("No message where received yet").toString
      case _ =>
        messages
          .map((user, msg) => Layouts.message(user, msg).toString)
          .mkString
    }

    cask.Ws.Text(text)
  }

  private def processMessage(msg: String)(session: Session) = {
    // Parse the mention of me the message
    val (mentionedUser, content) = parseMessageMention(msg)

    mentionedUser match {
      // Mention of bot
      case Some("@bot") =>
        val tokens = tokenizerSvc.tokenize(content)

        try {
          val expr = new Parser(tokens).parsePhrases()
          processBotMention(session, content, mentionedUser, expr)
        } catch {
          case e: Exception =>
            jsonResponse(success = false, err = e.getMessage)
        }

      // Mention of any user
      case Some(anyUser) =>
        msgSvc.add(
          session.getCurrentUser.get,
          Layouts.messageContent(content, Some(anyUser))
        )
        jsonResponse(success = true)

      // No mention
      case None =>
        msgSvc.add(
          session.getCurrentUser.get,
          Layouts.messageContent(content, None)
        )
        jsonResponse(success = true)
    }
  }

  private def processBotMention(
      session: Session,
      content: String,
      mentionedUser: Option[String],
      expr: ExprTree
  ): ujson.Obj = {
    // Add the message of user
    val userMsgId = msgSvc.add(
      session.getCurrentUser.get,
      Layouts.messageContent(content, mentionedUser),
      mentionedUser,
      Some(expr)
    )

    // Identification message must not be analyzed to prevent changes of session user
    val messageContent = expr match {
      case Auth(username) =>
        Layouts.messageContent(s"Hello ${username}", None)
      case _ =>
        // Get the reply of the bot
        Layouts.messageContent(analyzerSvc.reply(session)(expr), None)
    }

    // Send the reply of the bot
    msgSvc.add(
      "bot",
      messageContent,
      session.getCurrentUser,
      None,
      Some(userMsgId)
    )

    jsonResponse(success = true)
  }

  /** Split the mention of a user in a message and the content of the message
    * @param msg
    *   the message
    * @return
    *   a tuple with the mentioned user (if any) and the content of the message
    */
  private def parseMessageMention(msg: String): (Option[String], String) = {
    msg(0) match {
      case '@' =>
        val (mentionedUser, content) = msg.splitAt(msg.indexOf(' '))
        (Some(mentionedUser), content)
      case _ => (None, msg)
    }
  }

  private def jsonResponse(success: Boolean, err: String = ""): ujson.Obj = {
    ujson.Obj(
      "success" -> success,
      "err" -> err
    )
  }

  @cask.websocket("/subscribe")
  def subscribe(): WsHandler = {
    cask.WsHandler { chan =>
      subscribers += chan

      notifySubscribers(20)

      cask.WsActor { case cask.Ws.Close(_, _) =>
        subscribers -= chan
      }
    }
  }

  @cask.get("/clearHistory")
  def clearHistory(): Response[String] = {
    msgSvc.deleteHistory()
    cask.Redirect("/")
  }

  initialize()
end MessagesRoutes
