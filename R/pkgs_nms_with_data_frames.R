#' Get packages with data.frames
#'
#' @return A named vector of packages with `data.frame` objects
#' @export pkg_nms_with_data_frames
#'
#' @importFrom purrr map_vec
#'
#' @examples
#' pkg_nms_with_data_frames()
pkg_nms_with_data_frames <- function() {
  all_pkgs <- search_list_pkg_nms()
  df_pkg_set <- purrr::map_vec(.x = all_pkgs, check_data_frame_in_pkg)
  df_pkgs <- all_pkgs[df_pkg_set]
  return(df_pkgs)
}
