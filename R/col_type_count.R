#' Get number of data.frame columns by type
#'
#' @param df `data.frame` or `tibble`
#' @param type column type, one of: "num", "chr", "fct", "log",
#' "lst".
#'
#' @return count of columns by type
#' @export col_type_count
#'
#' @examples
#' col_type_count(mtcars, "num")
col_type_count <- function(df, type) {
  switch(EXPR = type,
    chr = ncol(dplyr::select(df, dplyr::where(is.character))),
    num = ncol(dplyr::select(df, dplyr::where(is.numeric))),
    fct = ncol(dplyr::select(df, dplyr::where(is.factor))),
    log = ncol(dplyr::select(df, dplyr::where(is.logical))),
    lst = ncol(dplyr::select(df, dplyr::where(is.list)))
    )
}
