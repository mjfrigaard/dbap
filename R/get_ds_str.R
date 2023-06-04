#' Get dataset structures from R package
#'
#' @param pkg name of package(s)
#'
#' @return `tibble` with the following columns:
#'
#' \describe{
#'   \item{Package}{name of package}
#'   \item{Dataset}{name of dataset}
#'   \item{Class}{class of dataset}
#'   \item{Columns}{total columns}
#'   \item{Rows}{total rows}
#'   \item{Logical}{total logical columns}
#'   \item{Numeric}{total numeric columns}
#'   \item{Character}{total character columns}
#'   \item{Factor}{total factor columns}
#'   \item{List}{total list columns}
#' }
#'
#' @export pkg_data_strs
#'
#' @importFrom purrr map2 map
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate filter
#'
#' @examples
#' require(forcats)
#' pkg_data_strs("forcats")
#' require(dplyr)
#' pkg_data_strs(c("dplyr", "forcats"))
pkg_data_strs <- function(pkg) {

  ds <- get_data_results(pkg = pkg)

  ds_list <- purrr::map2(
    .x = ds[["Item"]], .y = ds[["Package"]],
    .f = get_data, .progress = TRUE
  )

  class_tbl <-   dplyr::mutate(ds,
    Class = purrr::map(.x = ds_list, .f = class) |>
            purrr::map(paste0, collapse = ", ") |> unlist(),
    Columns = purrr::map(.x = ds_list, .f = ncol) |>
              purrr::map(paste0, " columns") |> unlist(),
    Rows = purrr::map(.x = ds_list, .f = nrow) |>
           purrr::map(paste0, " rows") |> unlist(),
    Logical = purrr::map(.x = ds_list,
                         .f = get_df_col_count, "log") |> unlist(),
    Numeric = purrr::map(.x = ds_list,
                         .f = get_df_col_count, "num") |> unlist(),
    Character = purrr::map(.x = ds_list,
                         .f = get_df_col_count, "chr") |> unlist(),
    Factor = purrr::map(.x = ds_list,
                         .f = get_df_col_count, "fct") |> unlist(),
    List = purrr::map(.x = ds_list,
                         .f = get_df_col_count, "lst") |> unlist(),
  )

  pkg_tbls_dfs <- dplyr::filter(class_tbl,
    stringr::str_detect(Class, "data.frame")
  )

  return(pkg_tbls_dfs)
}
