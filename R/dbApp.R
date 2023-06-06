#' Debug App
#'
#' @description
#' Stand-alone app function for debugging app.
#'
#' @export dbApp
#'
#' @importFrom lobstr tree
#' @importFrom shiny fluidPage sidebarLayout reactiveValuesToList
#' @importFrom shiny sidebarPanel shinyApp h3 code
#' @importFrom shiny mainPanel verbatimTextOutput renderPrint
dbApp <- function() {
  library(tidyr)
  library(dplyr)
  library(forcats)
  library(lubridate)
  shiny::shinyApp(
    ui = appUI,
    server = appServer)
}
