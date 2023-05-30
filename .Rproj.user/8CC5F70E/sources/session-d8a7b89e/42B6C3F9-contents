#' Filter variables by function
#'
#' @param data `data.frame`/`tibble`
#' @param filter any `is.[type]` function (i.e., `is.numeric`,
#' `is.character`, `is.logical`, etc.)
#'
#' @return data.frame/tibble filtered by function
#' @export filter_vars_fun
#'
#' @importFrom rlang as_function
#'
filter_vars_fun <- function(data, filter) {
  fun <- rlang::as_function(filter)
  stopifnot(is.data.frame(data))
  stopifnot(is.function(fun))
  names(data)[vapply(data, fun, logical(1))]
}
