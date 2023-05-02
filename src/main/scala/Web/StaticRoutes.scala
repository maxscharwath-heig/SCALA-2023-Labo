package Web

/**
  * Assembles the routes dealing with static files.
  */
class StaticRoutes()(implicit val log: cask.Logger) extends cask.Routes:
  @cask.staticResources("/static")
  def staticResourceRoutes() = "./"

  initialize()
end StaticRoutes
