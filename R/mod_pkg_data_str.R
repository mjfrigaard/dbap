#' Package data structure UI module
#'
#' @param id namespaced module id
#'
#' @return shiny UI module
#' @export mod_pkg_data_str_ui
#'
#' @importFrom shiny NS tagList verbatimTextOutput
mod_pkg_data_str_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # shiny::verbatimTextOutput(ns("pkg_data_inputs")),
    shiny::h4("Package data structure"),
    shiny::tableOutput(ns("pkg_data_str"))
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
#' @importFrom shiny NS moduleServer reactive req renderTable
#' @importFrom shiny bindCache bindEvent observe
mod_pkg_data_str_server <- function(id, pkg_data) {

  shiny::moduleServer(id, function(input, output, session) {

    output$pkg_data_str <- shiny::renderTable({
        dplyr::filter(
          pkg_data_str(pkg = pkg_data()$pkg),
          Item == pkg_data()$ds)

        }) |>
        shiny::bindEvent(c(pkg_data()$pkg, pkg_data()$ds),
                          ignoreNULL = TRUE)

    # output$pkg_data_inputs <- shiny::renderPrint({
    #   print(str(pkg_data()$pkg), width = 50L, max.levels = NULL)
    # })

    })

}
