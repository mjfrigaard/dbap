#' Inverted versions of `%in%`
#'
#' @export
#'
#' @examples
#' 1 %nin% 1:10
#' "A" %nin% 1:10
`%nin%` <- function(x, table) {
  match(x, table, nomatch = 0) == 0
}


#' Custom `skimr::skim()` for numeric variables
#'
#' @description
#' The custom `skimr::skim_with()`
#'
#' @export df_skim
#'
#' @importFrom skimr skim_with skim
#' @examples
#' require(dplyr)
#' df_skim(mtcars)
#' df_skim(dplyr::starwars)
df_skim <- function(df) {
  skims <- list(numeric =
                sfl(min = ~ min(., na.rm = TRUE),
                    med = ~ median(., na.rm = TRUE),
                    p0 = NULL, p25 = NULL, p50 = NULL,
                    p75 = NULL, p100 = NULL,
                    max = ~ max(., na.rm = TRUE)),
                factor =
                    sfl(complete_rate = NULL,
                        ordered = NULL),
                character =
                    sfl(complete_rate = NULL,
                        min = NULL, max = NULL,
                        whitespace = NULL)
    )
  df_skim <- skimr::skim_with(!!!skims)
  df_skim(df)
}

#' Deconstruct R objects
#'
#' @param x R object passed to `dput()`
#' @param quotes include quotes in the output
#' @param console logical, used in the console? If `FALSE`, then output is printed
#' with `base::noquote()`. If `TRUE`, output is returned with `cat()`
#'
#' @return Deparsed object
#' @export deconstruct
#'
#'
#' @examples
#' x <- deconstruct(names(mtcars), return = TRUE)
#' x
#' deconstruct(names(mtcars))
deconstruct <- function(x, return = FALSE, quote = TRUE) {
  raw_obj <- capture.output(dput(x, control = "all"))
  if (isFALSE(quote)) {
    obj_noquote <- gsub(pattern = '"', replacement = "", x = raw_obj)
    decon_noquote <- paste0(obj_noquote, collapse = "")
    decon_obj <- gsub("\\s+", " ", decon_noquote)
  } else {
    obj_quote <- gsub(pattern = '"', replacement = "'", x = raw_obj)
    decon_quote <- paste0(obj_quote, collapse = "")
    decon_obj <- gsub("\\s+", " ", decon_quote)
  }
  if (isFALSE(return)) {
    base::cat(decon_obj)
  } else {
    return(noquote(decon_obj))
  }
}
