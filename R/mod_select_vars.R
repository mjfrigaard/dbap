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
  shiny::selectInput(
    ns("fun"),
    label = "Filter by",
    choices = c('Numeric' = "is.numeric",
                'Character' = "is.character",
                'Factor' = "is.factor",
                'Logical' = "is.logical",
                'List' = "is.list"),
    selected = c('Numeric' = "is.numeric")),
  shiny::selectizeInput(
    ns("vars"),
    label = "Select variables",
    choices = NULL,
    multiple = TRUE,
    width = '90%'),
    shiny::verbatimTextOutput(ns("pkg_data"))
    )
}

#' Select variable server module
#'
#' @param id namespaced module id
#'
#' @return shiny server module
#' @export mod_select_vars_server
#'
#' @importFrom tibble as_tibble
#' @importFrom shiny NS moduleServer reactive req
#' @importFrom shiny bindCache bindEvent observe
mod_select_vars_server <- function(id, pkg_data) {

  shiny::moduleServer(id, function(input, output, session) {

    # output$pkg_data <- shiny::renderPrint({
    #   print(str(pkg_ds()),
    #     width = 50L, max.levels = NULL)
    # })

    pkg_ds <- shiny::reactive({
        pkg_data_object(ds = pkg_data()$ds,
                        pkg = pkg_data()$pkg)
        }) |>
          shiny::bindEvent(
            c(pkg_data()$ds, pkg_data()$pkg),
            ignoreNULL = TRUE)

      shiny::observe({
        filtered <- pull_type_cols(
                              data = pkg_ds(),
                              filter =  input$fun)
         shiny::updateSelectizeInput(session,
            inputId = "vars",
            choices = filtered,
           selected = filtered)
         }) |>
          shiny::bindEvent(c(pkg_ds(), input$fun),
                            ignoreNULL = TRUE)

        shiny::reactive({
           shiny::req(input$vars, input$fun)
              pkg_ds()[input$vars]
            }) |>
          shiny::bindEvent(input$vars, input$fun)



    })

}
