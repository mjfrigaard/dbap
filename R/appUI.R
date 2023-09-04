#' App UI
#'
#' Application standalone app function
#'
#' @export appUI
appUI <- function() {
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          mod_pkg_data_ui("data")
          ),
        mainPanel(
          br(),
          fluidRow(
            column(width = 12,
              mod_pkg_data_str_ui("str"))
          ),
          mod_select_vars_ui("vars"),
          verbatimTextOutput("skim"),
          verbatimTextOutput("ids")
        )
      )
    )
  )
}



