package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import Data.Session

/** Assembles the method used to layout ScalaTags
  */
object Layouts:
  private def htmlHead() = head(
    script(src := "static/js/main.js"),
    link(rel := "stylesheet", href := "static/css/main.css")
  )

  private def navbar(username: Option[String]) = tag("nav")(
    a(href := "/", cls := "nav-brand")("Bot-tender"),
    div(cls := "nav-item")(
      username
        .map(u => a(href := "/logout")("Logout"))
        .getOrElse(a(href := "/login")("Login"))
    )
  )

  private def navLink(to: String, label: String) = a(href := to)(label)

  private def messageForm() = form(
    id := "msgForm",
    onsubmit := "submitMessageForm();return false"
  )(
    div(id := "errorDiv", cls := "errorMsg"),
    label(`for` := "messageInput")("Your message:"),
    input(
      `type` := "text",
      id := "messageInput",
      name := "msg",
      placeholder := "Write your message"
    ),
    input(`type` := "submit", value := "Envoyer")
  )

  // Notice: Using Any for now, but we will use a proper type later,
  // as there is no need to display message for this part of lab
  private def messageList(messages: List[Any]) = div(id := "boardMessage")(
    if messages.isEmpty then "Please wait, the messages are loading !"
    else messages.map(_ => message("author", "content", "mention"))
  )

  private def message(author: String, content: String, mention: String) =
    div(cls := "msg")(
      span(cls := "author")(author),
      span(cls := "msg-content")(
        span(cls := "mention")(mention),
        content
      )
    )

  /** Renders the index page.
    */
  def index(session: Session) = html(
    htmlHead(),
    body(
      navbar(session.getCurrentUser),
      div(cls := "content")(
        messageList(List()),
        messageForm()
      )
    )
  )
end Layouts
