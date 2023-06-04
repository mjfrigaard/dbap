#' Get dataset metadata from packages
#'
#' @param pak name of package(s)
#'
#' @return metadata table of datasets in package(s)
#'
#' @export get_ds_metadata
#'
#' @importFrom purrr map list_rbind
#'
#' @examples
#' require(lubridate)
#' require(forcats)
#' require(tidyr)
#' pkgs <- c("lubridate", "forcats")
#' get_ds_metadata(pak = pkgs)
#' get_ds_metadata("tidyr")
get_ds_metadata <- function(pak) {

  if (length(pak) > 1) {

    purrr::map(
      .x = pak,
      .f = pkg_data_strs
    ) |>
      purrr::list_rbind()

  } else {

    pkg_data_strs(pkg = pak)

  }
}
