#' App Server
#'
#' @importFrom shiny reactive renderPrint reactiveValuesToList
#' @importFrom lobstr tree
#'
#' @export appServer
appServer <- function(input, output, session) {

  data_pkg <- mod_pkg_data_server("data")

  vars_select <- mod_select_vars_server("vars", pkg_data = data_pkg)

  mod_pkg_data_str_server("str", pkg_data = data_pkg)

  # output$ids <- shiny::renderPrint({
  #     print(str(vars_select()),
  #       width = 50L, max.levels = NULL)
  #   })

  output$skim <- shiny::renderPrint({ df_skim(df = vars_select()) })



}
