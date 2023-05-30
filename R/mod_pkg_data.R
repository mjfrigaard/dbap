#' Package dataset UI
#'
#' @param id namespaced module id
#'
#' @return shiny UI module
#' @export mod_pkg_data_ui
#'
#' @importFrom shiny NS tagList code selectInput
mod_pkg_data_ui <- function(id) {
  require(palmerpenguins)
  require(dplyr)
  df_pkgs <- get_pkgs_with_dfs()
  ns <- shiny::NS(id)
  shiny::tagList(
  shiny::selectInput(ns("pkg"),
    label = "Select a package",
    choices = df_pkgs),
  shiny::selectInput(ns("data"),
    label = "Select data",
    choices = NULL)
    )
}

#' Package dataset server module
#'
#' @param id namespaced module id
#'
#' @return shiny module server function
#' @export mod_pkg_data_server
#'
#' @importFrom shiny NS moduleServer reactive req bindEvent
#' @importFrom shiny updateSelectInput bindCache
mod_pkg_data_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
        pkg_data_nms <- get_pkg_df_names(pkg = input$pkg)
         shiny::updateSelectInput(session,
            inputId = "data",
            choices = pkg_data_nms)
         }) |>
       shiny::bindEvent(input$pkg,
         ignoreNULL = TRUE)

    shiny::reactive({
          shiny::req(input$data, input$pkg)
          get(x = input$data,
              pos = paste0("package:", input$pkg))
          }) |>
            shiny::bindCache(c(input$pkg, input$data)) |>
            shiny::bindEvent(input$data,
              ignoreNULL = TRUE)

  })

}
