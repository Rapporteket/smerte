#' Run the smerte Shiny Application
#'
#'
#' @return An object representing the smerte app
#' @export
smerteApp <- function() {

  rapbase::loggerSetup()

  shiny::shinyApp(
    ui = appUi,
    server = smerte::appServer
  )
}
