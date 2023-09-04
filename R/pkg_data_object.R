#' Get dataset from package
#'
#' @param ds name of dataset
#' @param pkg name of package
#'
#' @return data object
#'
#' @export pkg_data_object
#'
#'
#' @examples
#' pkg_data_object("gss_cat", "forcats")
#' pkg_data_object("starwars", "dplyr")
pkg_data_object <- function(ds, pkg) {
  objname <- stringr::str_remove_all(ds, " .*")
  e <- loadNamespace(pkg)
  if (!exists(ds, envir = e)) {
    dataname <- stringr::str_replace_all(ds, "^.*\\(|\\)$", "")
    e <- new.env()
    data(list = dataname, package = pkg, envir = e)
  }
  data_obj <- get(objname, envir = e)
  return(data_obj)
}
