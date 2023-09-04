#' Check if package is installed, if not install it
#'
#' @param pkg name of package (a character vector)
#'
#' @export check_pkg_inst
#'
#' @description
#' Check if `pkg` is installed with `require()`. If not, package is
#' installed with `install.packages(quietly = TRUE)`.
#'
#'
#' @examples
#' # remove.packages('janitor')
#' check_pkg_inst("janitor")
check_pkg_inst <- function(pkg) {
  libs <- as.character(pkg)
  req <- purrr::map_vec(.x = libs, .f = require,
                       quietly = TRUE,
                       character.only = TRUE)
  need <- libs[req == FALSE]
  if (length(need) > 0) {
    suppressWarnings(
     purrr::walk(.x = need,
                .f = install.packages,
                quiet = TRUE, verbose = FALSE)
    )
     purrr::map(.x = need,
                .f = requireNamespace,
                quietly = TRUE)
  }
}
