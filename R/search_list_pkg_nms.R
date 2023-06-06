#' Get all packages on search list
#'
#' @return All items from `search()` with a `package:` prefix
#' @export search_list_pkg_nms
#'
#' @description
#' This function is meant to be used in combination with `check_data_frame_in_pkg()`
#'
#' @seealso [check_data_frame_in_pkg()]
#'
#'
#' @examples
#' search_list_pkg_nms()
#'
search_list_pkg_nms <- function() {
  all_srch_lst <- search()
  all_pkgs <- grep(pattern = "package:", x = all_srch_lst, value = TRUE)
  pkgs <- gsub(pattern = ".*:|.GlobalEnv|datasets",
      replacement = "",
      x = all_pkgs)
  pkgs_chr <- pkgs[nzchar(pkgs)]
  pkg_nms <- `names<-`(pkgs_chr, pkgs_chr)
  return(pkg_nms)
}
