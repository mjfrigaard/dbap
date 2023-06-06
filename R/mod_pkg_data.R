#' Package dataset UI
#'
#' @param id namespaced module id
#'
#' @return shiny UI module
#' @export mod_pkg_data_ui
#'
#' @importFrom shiny NS tagList code selectInput
mod_pkg_data_ui <- function(id) {

  pkgs <- c("lubridate", "forcats", "tidyr", "dplyr")
  requireNamespace(pkgs)

  df_pkgs <- pkg_nms_with_data_frames()
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
#' @importFrom lobstr tree
#' @importFrom shiny NS moduleServer reactive bindEvent
#' @importFrom shiny updateSelectInput bindCache
#' @importFrom shiny req renderPrint
#' @importFrom renv install
mod_pkg_data_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
          shiny::req(input$pkg)
        pkg_data_nms <- pkg_data_frame_nms(pkg = input$pkg)
         shiny::updateSelectInput(session,
            inputId = "data",
            choices = pkg_data_nms)
         }) |>
       shiny::bindEvent(input$pkg)

    shiny::reactive({
          shiny::req(input$data, input$pkg)
          list(ds = input$data,
               pkg = input$pkg)
          }) |>
            shiny::bindCache(c(input$pkg, input$data)) |>
            shiny::bindEvent(input$data,
              ignoreNULL = TRUE)

  })

}
