package Web

/**
  * Assembles the routes dealing with static files.
  */
class StaticRoutes()(implicit val log: cask.Logger) extends cask.Routes:
  // Serve the resources in the `./static` directory
  @cask.staticResources("/static")
  def staticResourceRoutes() = "./"

  initialize()
end StaticRoutes
