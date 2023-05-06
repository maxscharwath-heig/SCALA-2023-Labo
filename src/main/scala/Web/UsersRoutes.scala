package Web

import Data.{AccountService, SessionService, Session}

/** Assembles the routes dealing with the users:
  *   - One route to display the login form and register form page
  *   - One route to process the login form and display the login success page
  *   - One route to process the register form and display the register success
  *     page
  *   - One route to logout and display the logout success page
  *
  * The username of the current session user is stored inside a cookie called
  * `username`.
  */
class UsersRoutes(accountSvc: AccountService, sessionSvc: SessionService)(
    implicit val log: cask.Logger
) extends cask.Routes:
  import Decorators.getSession

  @getSession(sessionSvc)
  @cask.get("/login")
  def index()(session: Session) = Layouts.login(session)

  @getSession(sessionSvc)
  @cask.postForm("/login")
  def login(username: cask.FormValue)(session: Session) = {
    if (!accountSvc.isAccountExisting(username.value)) {
      // TODO Error
      println("Error")
    }

    session.setCurrentUser(username.value)

    Layouts.success("You have been logged in successfully")
  }

  @getSession(sessionSvc)
  @cask.postForm("/register")
  def register(username: cask.FormValue)(session: Session) = {
    if (accountSvc.isAccountExisting(username.value)) {
      // TODO Error
      println("Error")
    }
    accountSvc.addAccount(username.value, 30.0)

    session.setCurrentUser(username.value)

    Layouts.success("You have been registered successfully")
  }

  @getSession(sessionSvc)
  @cask.get("/logout")
  def logout()(session: Session) = {
    session.reset()
    Layouts.success("You have been logged out successfully")
  }

  initialize()
end UsersRoutes
