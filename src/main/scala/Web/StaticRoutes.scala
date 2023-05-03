package Web

/**
  * Assembles the routes dealing with static files.
  */
class StaticRoutes()(implicit val log: cask.Logger) extends cask.Routes:
  // Serve the resources in the `./static/js` directory
  @cask.staticResources("/static/js")
  def staticResourceRoutesJS() = "./js"

  // Serve the resources in the `./static/css` directory
  @cask.staticResources("/static/css")
  def staticResourceRoutesCSS() = "./css"

  initialize()
end StaticRoutes
