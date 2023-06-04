#' Check if package is loaded, if not load it
#'
#' @param pkg name of package (a character vector)
#' @param quiet logical, print messages
#'
#' @return Package: `'name'` loaded or Loading package: `'name'`
#' @export check_inst_pkg
#'
#' @description
#' Check if `pkg` is installed with `isNamespaceLoaded()`. If not, package is
#' installed with `requireNamespace(quietly = TRUE)`.
#'
#'
#' @examples
#' # remove from ns
#' unloadNamespace("fs")
#' unloadNamespace("box")
#' # with single pkg
#' check_inst_pkg("fs")
#' # remove again
#' unloadNamespace("fs")
#' # with multiple pkgs
#' pkgs <- c("fs", "box")
#' check_inst_pkg(pkgs)
check_inst_pkg <- function(pkg, quiet = FALSE) {
  if (isFALSE(quiet)) {
    # with messages
    if (!isNamespaceLoaded(pkg)) {
      if (requireNamespace(pkg, quietly = FALSE)) {
        cat(paste0("Loading package: ", pkg, "\n"))
      } else {
        stop(paste0(pkg, " not available"))
      }
    } else {
      cat(paste0("Package ", pkg, " loaded\n"))
    }
  } else {
    # without messages
    if (!isNamespaceLoaded(pkg)) {
      if (requireNamespace(pkg, quietly = TRUE)) {
      } else {
        stop(paste0(pkg, " not available"))
      }
    }
  }
}
