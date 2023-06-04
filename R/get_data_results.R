#' Get results from `data(package = [package])`
#'
#' @param pkg name of package
#'
#' @return metadata table of data object
#'
#' @export get_data_results
#'
#'
#' @importFrom tibble as_tibble
#'
#' @examples
#' get_data_results("lubridate")
#' get_data_results("dplyr")
get_data_results <- function(pkg) {
  # load packages
  check_inst_pkg(pkg = pkg, quiet = TRUE)

  tibble::as_tibble(
    data.frame(
      Package = data(package = pkg)$results[, "Package"],
      Item = data(package = pkg)$results[, "Item"],
      Title = data(package = pkg)$results[, "Title"],
      stringsAsFactors = FALSE,
      check.names = FALSE,
      row.names = NULL
    )
  )
}
