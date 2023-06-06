#' Get names of data.frames from package
#'
#' @param pkg name of package
#'
#' @return named vector of data.frames in package
#' @export pkg_data_frame_nms
#'
#' @examples
#' pkg_data_frame_nms(pkg = "base")
#' pkg_data_frame_nms(pkg = "datasets")
#' pkg_data_frame_nms(pkg = "lubridate")
pkg_data_frame_nms <- function(pkg) {
  check_pkg_inst(pkg)
  check_pkg_ns(pkg, quiet = TRUE)
  pkg_pos <- paste0("package:", pkg)
  pkg_nms <- ls(pkg_pos)
  pkg_objects <- purrr::map2(.x = pkg_nms, .y = pkg_pos, .f = get)
  df_set <- purrr::map_vec(.x = pkg_objects,.f = is.data.frame)
  df_names <- pkg_nms[df_set]
  if (length(df_names) > 0) {
    return(df_names)
  }
  return(NULL)
}

