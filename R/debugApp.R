#' Debug App
#'
#' @description
#' Stand-alone app function for debugging app.
#'
#' @export debugApp
#'
#' @importFrom lobstr tree
#' @importFrom shiny fluidPage sidebarLayout reactiveValuesToList
#' @importFrom shiny sidebarPanel shinyApp h3 code
#' @importFrom shiny mainPanel verbatimTextOutput renderPrint
debugApp <- function() {
  require(palmerpenguins)
  require(Lahman)
  require(dplyr)
  require(NHANES)
  shiny::shinyApp(
    ui = appUI,
    server = appServer)
}
