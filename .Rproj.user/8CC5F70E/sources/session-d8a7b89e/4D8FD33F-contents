#' App Server
#'
#' @importFrom shiny reactive renderPrint reactiveValuesToList
#'
#' @importFrom lobstr tree
#'
#' @export appServer
appServer <- function(input, output, session) {

    data_pkg <- mod_pkg_data_server("data")

    vars_select <- mod_select_vars_server("vars", pkg_data = data_pkg)

    output$skim <- shiny::renderPrint({ df_skim(df = vars_select()) })

    output$vals <- shiny::renderPrint({
        vals <- shiny::reactiveValuesToList(input, TRUE)
        lobstr::tree(vals)
      })

}
