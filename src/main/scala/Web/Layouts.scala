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
      .map(u => s"You are logged in as ${u}")
      .getOrElse("You are not logged in"),
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

  /** Render the login form.
    */
  private def loginForm(error: Option[String]) =
    form(id := "loginForm", method := "POST", action := "/login")(
      div(id := "errorLoginDiv", cls := "errorMsg")(error.getOrElse("")),
      label(`for` := "usernameLoginInput")("Username:"),
      input(
        `type` := "text",
        id := "usernameLoginInput",
        name := "username",
        placeholder := "Enter your username"
      ),
      input(`type` := "submit", value := "Envoyer")
    )

    /** Render the register form.
      */
  private def registerForm(error: Option[String]) =
    form(id := "registerForm", method := "POST", action := "/register")(
      div(id := "errorRegisterDiv", cls := "errorMsg")(error.getOrElse("")),
      label(`for` := "usernameRegisterInput")("Username:"),
      input(
        `type` := "text",
        id := "usernameRegisterInput",
        name := "username",
        placeholder := "Enter your username"
      ),
      input(`type` := "submit", value := "Envoyer")
    )

  /** Render a chat message.
    */
  def message(author: String, content: Frag): Frag =
    div(cls := "msg")(
      tag("span")(cls := "author")(author),
      content
    )

  def messageContent(message: String, mention: Option[String]): Frag =
    tag("span")(cls := "msg-content")(
      span(cls := "mention")(mention),
      message
    )

  /** Render a placeholder message.
    */
  def placeholderContent(placeholder: String) =
    p(textAlign.center)(placeholder)

  /** Renders the index page.
    */
  def index(session: Session) = html(
    htmlHead(),
    body(
      navbar(session.getCurrentUser),
      div(cls := "content")(
        div(id := "boardMessage")(),
        messageForm()
      )
    )
  )

  /** Renders the login page.
    */
  def login(loginError: Option[String], registerError: Option[String])(
      session: Session
  ) = html(
    htmlHead(),
    body(
      // TODO: remove duplication with navbar
      tag("nav")(
        a(href := "/", cls := "nav-brand")("Bot-tender"),
        navLink("/", "Go to the message board")
      ),
      div(cls := "content")(
        h1("Login"),
        loginForm(loginError),
        h1("Register"),
        registerForm(registerError)
      )
    )
  )

  /** Renders the success page.
    */
  def success(message: String) = html(
    htmlHead(),
    body(
      div(cls := "content")(
        h1(s"Success: ${message}"),
        a(href := "/")("Back to home")
      )
    )
  )
end Layouts
