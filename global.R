


pkgs <- c("askpass", "base64enc", "brio", "bslib", "cachem", "callr",
"cli", "colorspace", "commonmark", "config", "cpp11", "crayon",
"curl", "desc", "diffobj", "digest", "dplyr", "ellipsis", "evaluate",
"fansi", "farver", "fastmap", "fontawesome", "fs", "generics",
"ggplot2", "glue", "gtable", "highr", "hms", "htmltools", "httpuv",
"isoband", "janitor", "jquerylib", "jsonlite", "knitr", "labeling",
"later", "lifecycle", "lubridate", "magrittr", "memoise", "mime",
"munsell", "NHANES", "openssl", "packrat", "palmerpenguins",
"pillar", "pkgconfig", "pkgload", "praise", "processx", "promises",
"ps", "purrr", "R6", "rappdirs", "RColorBrewer", "Rcpp", "rlang",
"rmarkdown", "rprojroot", "rsconnect", "rstudioapi", "sass",
"scales", "shiny", "shinytest2", "snakecase", "sourcetools",
"stringi", "stringr", "sys", "testthat", "tibble", "tidyr", "tidyselect",
"timechange", "tinytex", "utf8", "vctrs", "vdiffr", "viridisLite",
"waldo", "withr", "xfun", "xml2", "xtable", "yaml", "zeallot"
)

# pkgs <- sort(unique(pkgs))
# dput(pkgs)

renv::install(pkgs, prompt = FALSE)
# renv::update(pkgs, prompt = FALSE)
