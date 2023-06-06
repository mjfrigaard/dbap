#' Check if package contains data.frames
#'
#' @param pkg name of package (a character vector)
#'
#' @return logical (`TRUE` = has data.frame, `FALSE` = no data.frame)
#'
#' @export check_data_frame_in_pkg
#'
#' @description
#' Returns `TRUE` if package has `data.frame`. If package is not installed,
#' install with `install.packages(dependencies = TRUE)`.
#'
#' 1. Check if the package is installed and load it
#'
#' 2. Retrieve the objects in the package
#'
#' 3. Use `purrr::map_lgl()` to apply `is.data.frame()` to each object in the
#'  package. `map_lgl()` returns a logical vector with the same length as
#'  the retrieved package objects.
#'
#'
#' @seealso [check_pkg_inst()]
#'
#' @importFrom purrr map_lgl
#'
#' @examples
#' check_data_frame_in_pkg("dplyr")
#' check_data_frame_in_pkg("stringr")
check_data_frame_in_pkg <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    check_pkg_inst(pkg = pkg)
  }
  check_pkg_ns(pkg, quiet = TRUE)
  pkg_obj <- ls(paste("package", pkg, sep = ":"))
  is_df <- purrr::map_lgl(.x = pkg_obj,
    ~ is.data.frame(get(x = .x,
      envir = as.environment(
        paste("package", pkg, sep = ":")
      ))))
  return(any(is_df))
}
