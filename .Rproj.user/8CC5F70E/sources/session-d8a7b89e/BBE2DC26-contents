#' pkgDataApp
#'
#' @return shiny app
#' @export pkgDataApp
#' @import NHANES
#' @import palmerpenguins
#' @importFrom shiny shinyApp fluidPage
#' @importFrom shiny sidebarLayout sidebarPanel
#' @importFrom shiny mainPanel
pkgDataApp <- function() {
    require(janitor)
    require(palmerpenguins)
    require(NHANES)
    shiny::shinyApp(
      ui = appUI,
      server = appServer)

}
