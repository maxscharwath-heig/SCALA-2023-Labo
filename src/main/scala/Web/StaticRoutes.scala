package Web

/**
  * Assembles the routes dealing with static files.
  */
class StaticRoutes()(implicit val log: cask.Logger) extends cask.Routes:
  // Serve the resources folder contained in the `main/resources` directory
  // under the /static route so every file ressource files in the folder will be accessible.
  @cask.staticResources("/static")
  def staticResourceRoutes() = "./"

  initialize()
end StaticRoutes
