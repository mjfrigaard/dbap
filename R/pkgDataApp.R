#' Debug App
#'
#' @description
#' Stand-alone app function for debugging app.
#'
#' @export pkgdataApp
#'
pkgdataApp <- function() {
  require(tidyr)
  require(dplyr)
  require(forcats)
  require(lubridate)
  shiny::shinyApp(
    ui = appUI,
    server = appServer)
}
