#' Package dataset UI
#'
#' @param id namespaced module id
#'
#' @return shiny UI module
#' @export mod_pkg_data_ui
#'
mod_pkg_data_ui <- function(id) {

  pkgs <- c("lubridate", "forcats", "tidyr", "dplyr")
  requireNamespace(pkgs)

  df_pkgs <- pkg_nms_with_data_frames()
  ns <- NS(id)
  tagList(
  selectInput(ns("pkg"),
    label = "Select a package",
    choices = df_pkgs),
  selectInput(ns("data"),
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
mod_pkg_data_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    observe({
          req(input$pkg)
        pkg_data_nms <- pkg_data_frame_nms(pkg = input$pkg)
         updateSelectInput(session,
            inputId = "data",
            choices = pkg_data_nms)
         }) |>
       bindEvent(input$pkg)

    reactive({
          req(input$data, input$pkg)
          list(ds = input$data,
               pkg = input$pkg)
          }) |>
            bindCache(c(input$pkg, input$data)) |>
            bindEvent(input$data,
              ignoreNULL = TRUE)

  })

}
