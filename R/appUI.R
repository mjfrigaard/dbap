#' App UI
#'
#' @importFrom shiny tagList fluidPage sidebarLayout
#' @importFrom shiny sidebarPanel mainPanel br h3 code
#' @importFrom shiny fluidRow code verbatimTextOutput
#'
#' @export appUI
appUI <- function() {
  shiny::tagList(
    shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mod_pkg_data_ui("data")
          ),
        shiny::mainPanel(
          shiny::h3(
            shiny::code("pkgDataApp()")),
          shiny::fluidRow(
            shiny::column(width = 12,
              mod_pkg_data_str_ui("str"))
          ),
          mod_select_vars_ui("vars"),
          shiny::verbatimTextOutput("skim"),
          shiny::verbatimTextOutput("ids")
        )
      )
    )
  )
}



