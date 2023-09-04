# run app for package
# options(shiny.testmode = TRUE)
pkgload::load_all(
  attach = TRUE, # If FALSE load_all() behaves like loadNamespace()
                 # If TRUE (the default), it behaves like library()
  export_all = FALSE, # If TRUE (the default), export all objects
  helpers = TRUE, # if TRUE loads testthat test helpers
  attach_testthat = TRUE
  )

library(dbap)
dbap::pkgdataApp()
