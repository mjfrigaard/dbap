#' Get results from `data(package = [package])`
#'
#' @param pkg name of package
#'
#' @return metadata table of data object
#'
#' @export get_data_results
#'
#'
#' @importFrom tibble as_tibble
#'
#' @examples
#' get_data_results("lubridate")
#' get_data_results("dplyr")
get_data_results <- function(pkg) {
  # make sure requested packages are available and loaded
  for (i in seq_along(pkg)) {
    if (!isNamespaceLoaded(pkg[i])) {
      if (requireNamespace(pkg[i], quietly = TRUE)) {
        cat(paste("Loading package:", pkg[i], "\n"))
      } else {
        stop(paste("Package", pkg[i], "is not available"))
      }
    }
  }
  dsitems <- data(package = pkg)$results
  ds <- tibble::as_tibble(
    data.frame(
      Package = dsitems[, "Package"],
      Item = dsitems[, "Item"],
      Title = dsitems[, "Title"],
      stringsAsFactors = FALSE,
      check.names = FALSE,
      row.names = NULL
    )
  )
  return(ds)
}


#' Get dataset from package
#'
#' @param x name of dataset
#' @param pkg name of package
#'
#' @return data object
#'
#' @export get_data
#'
#' @examples
#' get_data("lakers", "lubridate")
#' get_data("starwars", "dplyr")
get_data <- function(x, pkg) {
  objname <- gsub(" .*", "", x)
  e <- loadNamespace(pkg)
  if (!exists(x, envir = e)) {
    dataname <- sub("^.*\\(", "", x)
    dataname <- sub("\\)$", "", dataname)
    e <- new.env()
    data(list = dataname, package = pkg, envir = e)
  }
  get(objname, envir = e)
}

#' Get dataset metadata from packages
#'
#' @param pak name of package(s)
#'
#' @return metadata table of datasets in package(s)
#'
#' @export get_ds_metadata
#'
#' @importFrom purrr map map2 list_rbind
#' @importFrom dplyr mutate filter
#' @importFrom stringr str_detect
#'
#' @examples
#' require(lubridate)
#' require(forcats)
#' require(tidyr)
#' pkgs <- c("lubridate", "forcats")
#' get_ds_metadata(pak = pkgs)
#' get_ds_metadata("tidyr")
get_ds_metadata <- function(pak) {
  get_data_obj_metadata <- function(pkg) {
    ds <- get_data_results(pkg = pkg)
    ds_list <- purrr::map2(
      .x = ds[["Item"]], .y = ds[["Package"]],
      .f = get_data, .progress = TRUE
    )
    class_tbl <- dplyr::mutate(ds,
      Class = purrr::map(.x = ds_list, .f = class) |>
        purrr::map(paste0, collapse = ", ") |>
        unlist(),
      Columns = purrr::map(.x = ds_list, .f = ncol) |>
        purrr::map(paste0, " columns") |>
        unlist(),
      Rows = purrr::map(.x = ds_list, .f = nrow) |>
        purrr::map(paste0, " rows") |>
        unlist()
    )
    pkg_tbls_dfs <- dplyr::filter(
      class_tbl,
      stringr::str_detect(Class, "data.frame")
    )

    return(pkg_tbls_dfs)
  }

  if (length(pak) > 1) {
    purrr::map(
      .x = pak,
      .f = get_data_obj_metadata
    ) |>
      purrr::list_rbind()
  } else {
    get_data_obj_metadata(pkg = pak)
  }
}
