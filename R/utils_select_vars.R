#' Filter variables by function
#'
#' @param data `data.frame`/`tibble`
#' @param filter any `is.[type]` function (i.e., `is.numeric`,
#' `is.character`, `is.logical`, etc.)
#'
#' @return data.frame/tibble filtered by function
#' @export pull_type_cols
#'
#'
pull_type_cols <- function(data, filter) {
  fun <- rlang::as_function(filter)
  stopifnot(is.data.frame(data))
  stopifnot(is.function(fun))
  names(data)[vapply(data, fun, logical(1))]
}
