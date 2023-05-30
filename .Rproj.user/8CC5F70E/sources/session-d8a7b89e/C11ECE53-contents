#' Select variables UI module
#'
#' @param id namespaced module id
#'
#' @return shiny UI module
#' @export mod_select_vars_ui
#'
#' @importFrom shiny NS tagList code selectInput
#' @importFrom shiny selectizeInput
mod_select_vars_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
  shiny::code("select vars"),
  shiny::selectInput(
    ns("fun"),
    label = "Filter by",
    choices = c("is.numeric",
                "is.character",
                "is.factor",
                "is.logical",
                "is.list"),
    selected = "is.numeric"),
  shiny::selectizeInput(
    ns("vars"),
    label = "Select variables",
    choices = NULL,
    multiple = TRUE)
    )
}

#' Select variable server module
#'
#' @param id namespaced module id
#'
#' @return shiny server module
#' @export mod_select_vars_server
#'
#'
#' @importFrom shiny NS moduleServer reactive req
#' @importFrom shiny bindCache bindEvent observe
mod_select_vars_server <- function(id, pkg_data) {

  shiny::moduleServer(id, function(input, output, session) {

      shiny::observe({
        filtered <- filter_vars_fun(
                              data = pkg_data(),
                              filter =  input$fun)
         shiny::updateSelectizeInput(session,
            inputId = "vars",
            choices = filtered,
           selected = filtered)
         }) |>
          shiny::bindEvent(c(pkg_data(), input$fun),
            ignoreNULL = TRUE)

        shiny::reactive({
           shiny::req(input$vars, input$fun)
              pkg_data()[input$vars]
            }) |>
          shiny::bindEvent(input$vars, input$fun)

    })

}
