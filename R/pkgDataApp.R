#' Debug App
#'
#' @description
#' Stand-alone app function for debugging app.
#'
#' @export pkgDataApp
#'
#' @importFrom lobstr tree
#' @importFrom shiny fluidPage sidebarLayout reactiveValuesToList
#' @importFrom shiny sidebarPanel shinyApp h3 code
#' @importFrom shiny mainPanel verbatimTextOutput renderPrint
pkgDataApp <- function() {
  require(tidyr)
  require(dplyr)
  require(forcats)
  require(lubridate)
  shiny::shinyApp(
    ui = appUI,
    server = appServer)
}
