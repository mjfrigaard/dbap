pkgs <- c("config", "dplyr", "devtools", "forcats", "fs", "ggplot2", "glue",
  "janitor", "knitr", "lobstr", "lubridate", "pkgload", "purrr",
  "rlang", "rmarkdown", "shiny", "shinytest2", "skimr", "snakecase",
  "stringr", "testthat", 'tibble', "tidyr", "vdiffr", "yaml", "zeallot")

# pkgs <- sort(unique(pkgs))
# dput(pkgs)

renv::install(pkgs, prompt = FALSE)
# renv::update(pkgs, prompt = FALSE)
