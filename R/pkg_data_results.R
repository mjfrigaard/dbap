#' Get results from `data(package = [package])`
#'
#' @param pkg name of package
#'
#' @return metadata table of data object
#'
#' @export pkg_data_results
#'
#'
#' @importFrom tibble as_tibble
#'
#' @examples
#' pkg_data_results("lubridate")
#' pkg_data_results("dplyr")
pkg_data_results <- function(pkg) {
  # load packages
  check_pkg_ns(pkg = pkg, quiet = TRUE)

  results <- tibble::as_tibble(
    data.frame(
      Package = data(package = pkg)$results[, "Package"],
      Item = data(package = pkg)$results[, "Item"],
      Title = data(package = pkg)$results[, "Title"],
      stringsAsFactors = FALSE,
      check.names = FALSE,
      row.names = NULL
    )
  )

  if (nrow(results) == 0) {

  data_results <- tibble::as_tibble(
    data.frame(
		matrix(
			nrow = 1, ncol = 11,
			byrow = TRUE,
			dimnames = list(NULL,
			  c("Package", "Item", "Title",
			    "Class", "Columns", "Rows",
			    "Logical", "Numeric",
			    "Character", "Factor",
			    "List"))
				),
		row.names = NULL))

    return(data_results)


  } else {

    results

  }

}
