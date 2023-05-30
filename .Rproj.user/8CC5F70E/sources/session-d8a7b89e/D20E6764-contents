#' Custom `skimr::skim` App
#'
#' @description
#' Stand-alone app function for custom `skimr::skim()` app.
#'
#' @export skimApp
#'
#' @importFrom lobstr tree
#' @importFrom shiny fluidPage sidebarLayout reactiveValuesToList
#' @importFrom shiny sidebarPanel shinyApp h3 code
#' @importFrom shiny mainPanel verbatimTextOutput renderPrint
skimApp <- function() {
  require(palmerpenguins)
  require(Lahman)
  require(dplyr)
  require(NHANES)
  shiny::shinyApp(
    ui = appUI,
    server = appServer)
}
