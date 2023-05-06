package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import Data.Session
import scala.quoted.FromExpr.NilFromExpr

/** Assembles the method used to layout ScalaTags
  */
object Layouts:
  /** Render the HTML head.
    */
  private def htmlHead() = head(
    script(src := "static/js/main.js"),
    link(rel := "stylesheet", href := "static/css/main.css")
  )

  /** Render the navbar.
    * @param username
    *   Connected user's username, if any.
    */
  private def navbar(username: Option[String]) = tag("nav")(
    a(href := "/", cls := "nav-brand")("Bot-tender"),
    username
      .map(u => navLink("/logout", "Logout"))
      .getOrElse(navLink("/login", "Login"))
  )

  /** Render a link in the navbar.
    *
    * @param to
    *   the link target
    * @param label
    *   the link label
    */
  private def navLink(to: String, label: String) = div(cls := "nav-item")(
    a(href := to)(label)
  )

  /** Render the message form.
    */
  private def messageForm() = form(
    id := "msgForm",
    onsubmit := "submitMessageForm(); return false"
  )(
    div(id := "errorDiv", cls := "errorMsg")(),
    label(`for` := "messageInput")("Your message:"),
    input(
      `type` := "text",
      id := "messageInput",
      placeholder := "Write your message"
    ),
    input(`type` := "submit", value := "Envoyer")
  )

  /** Render a list of messages.
    *
    * Notice: Using Any for now, but we will use a proper type later, as there
    * is no need to display message for this part of lab.
    */
  private def messageList(messages: List[Any]) = div(id := "boardMessage")(
    if messages.isEmpty then
      placeholderContent("Please wait, the messages are loading !")
    else messages.map(_ => message("author", "content", "mention"))
  )

  /** Render a chat message.
    */
  private def message(author: String, content: String, mention: String) =
    div(cls := "msg")(
      span(cls := "author")(author),
      span(cls := "msg-content")(
        span(cls := "mention")(mention),
        content
      )
    )

  /** Render a placeholder message.
    */
  private def placeholderContent(placeholder: String) =
    p(textAlign.center)(placeholder)

  /** Renders the index page.
    */
  def index(session: Session) = html(
    htmlHead(),
    body(
      navbar(session.getCurrentUser),
      div(cls := "content")(
        messageList(Nil), // For now, we don't display any message
        messageForm()
      )
    )
  )
end Layouts
