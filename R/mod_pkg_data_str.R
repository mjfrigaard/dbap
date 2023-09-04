#' Package data structure UI module
#'
#' @param id namespaced module id
#'
#' @return shiny UI module
#' @export mod_pkg_data_str_ui
#'
mod_pkg_data_str_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # verbatimTextOutput(ns("pkg_data_inputs")),
    h4("Package data structure"),
    tableOutput(ns("pkg_data_str"))
    )
}

#' Select variable server module
#'
#' @param id namespaced module id
#'
#' @return shiny server module
#' @export mod_pkg_data_str_server
#'
#'
mod_pkg_data_str_server <- function(id, pkg_data) {

  moduleServer(id, function(input, output, session) {

    output$pkg_data_str <- renderTable({
        dplyr::filter(
          pkg_data_str(pkg = pkg_data()$pkg),
          Item == pkg_data()$ds)

        }) |>
        bindEvent(c(pkg_data()$pkg, pkg_data()$ds),
                          ignoreNULL = TRUE)

    # output$pkg_data_inputs <- renderPrint({
    #   print(str(pkg_data()$pkg), width = 50L, max.levels = NULL)
    # })

    })

}
