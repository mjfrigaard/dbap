#' Select variables UI module
#'
#' @param id namespaced module id
#'
#' @return shiny UI module
#' @export mod_select_vars_ui
#'
mod_select_vars_ui <- function(id) {
  ns <- NS(id)
  tagList(
  selectInput(
    ns("fun"),
    label = "Filter by",
    choices = c('Numeric' = "is.numeric",
                'Character' = "is.character",
                'Factor' = "is.factor",
                'Logical' = "is.logical",
                'List' = "is.list"),
    selected = c('Numeric' = "is.numeric")),
  selectizeInput(
    ns("vars"),
    label = "Select variables",
    choices = NULL,
    multiple = TRUE,
    width = '90%'),
    verbatimTextOutput(ns("pkg_data"))
    )
}

#' Select variable server module
#'
#' @param id namespaced module id
#'
#' @return shiny server module
#' @export mod_select_vars_server
#'
mod_select_vars_server <- function(id, pkg_data) {

  moduleServer(id, function(input, output, session) {

    # output$pkg_data <- renderPrint({
    #   print(str(pkg_ds()),
    #     width = 50L, max.levels = NULL)
    # })

    pkg_ds <- reactive({
        pkg_data_object(ds = pkg_data()$ds,
                        pkg = pkg_data()$pkg)
        }) |>
          bindEvent(
            c(pkg_data()$ds, pkg_data()$pkg),
            ignoreNULL = TRUE)

      observe({
        filtered <- pull_type_cols(
                              data = pkg_ds(),
                              filter =  input$fun)
         updateSelectizeInput(session,
            inputId = "vars",
            choices = filtered,
           selected = filtered)
         }) |>
          bindEvent(c(pkg_ds(), input$fun),
                            ignoreNULL = TRUE)

        reactive({
           req(input$vars, input$fun)
              pkg_ds()[input$vars]
            }) |>
          bindEvent(input$vars, input$fun)



    })

}
