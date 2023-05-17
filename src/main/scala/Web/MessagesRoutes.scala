package Web

import Chat.{AnalyzerService, TokenizerService}
import Data.{MessageService, AccountService, SessionService, Session}
import scala.collection.mutable.ListBuffer
import castor.Context.Simple.global

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

  val subscribers = ListBuffer[cask.endpoints.WsChannelActor]()

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

      msgSvc.add(session.getCurrentUser.get, Layouts.messageContent(msg))

      val latests = latestMessagesAsString(20)
      subscribers.foreach(_.send(cask.Ws.Text(latests)))

      ujson.Obj("success" -> true, "err" -> "")
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

  // TODO - Part 3 Step 5: Modify the code of step 4b to process the messages sent to the bot (message
  //      starts with `@bot `). This message and its reply from the bot will be added to the message
  //      store together.
  //
  //      The exceptions raised by the `Parser` will be treated as an error (same as in step 4b)

  initialize()
end MessagesRoutes
