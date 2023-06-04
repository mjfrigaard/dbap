#' Get dataset from package
#'
#' @param ds name of dataset
#' @param pkg name of package
#'
#' @return data object
#'
#' @export get_data
#'
#' @importFrom stringr str_remove_all str_replace_all
#'
#' @examples
#' get_data("gss_cat", "forcats")
#' get_data("starwars", "dplyr")
get_data <- function(ds, pkg) {
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
