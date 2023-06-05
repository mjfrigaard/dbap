#' Get dataset structures from R package
#'
#' @param pkg name of package(s)
#'
#' @return dataset structure `tibble`
#'
#' @section The returned object has the following columns:
#'
#' * `Package`: name of package
#'
#' * `Dataset`: name of dataset
#'
#' * `Class`: class of dataset
#'
#' * `Columns`: total columns
#'
#' * `Rows`: total rows
#'
#' * `Logical`: total logical columns
#'
#' * `Numeric`: total numeric columns
#'
#' * `Character`: total character columns
#'
#' * `Factor`: total factor columns
#'
#' * `List`: total list columns
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

  data_results <- get_data_results(pkg = pkg)

  if (!is.logical(data_results[["Item"]])) {
    # data_results contains data objects
    ds_list <- purrr::map2(
      .x = data_results[["Item"]], .y = data_results[["Package"]],
      .f = get_data, .progress = TRUE
    )

    class_tbl <- dplyr::mutate(data_results,
      Class = purrr::map(.x = ds_list, .f = class) |>
        purrr::map(paste0, collapse = ", ") |> unlist()
    )

    df_tbl <- dplyr::filter(
      class_tbl,
      stringr::str_detect(Class, "data.frame")
    )

    if (nrow(df_tbl) == 0) {
      # df_tbl does not contain 'data.frame' classes
      data_results <- tibble::as_tibble(
        data.frame(
          matrix(
            nrow = 1, ncol = 11,
            byrow = TRUE,
            dimnames = list(
              NULL,
              c(
                "Package", "Item", "Title",
                "Class", "Columns", "Rows",
                "Logical", "Numeric", "Character",
                "Factor", "List"
              )
            )
          ),
          row.names = NULL
        )
      )

      return(data_results)

    } else {

      # df_tbl contains 'data.frame' classes
      dplyr::mutate(df_tbl,
        Columns = purrr::map(.x = ds_list, .f = ncol) |>
          purrr::map(paste0, " columns") |> unlist(),
        Rows = purrr::map(.x = ds_list, .f = nrow) |>
          purrr::map(paste0, " rows") |> unlist(),
        Logical = purrr::map(
          .x = ds_list,
          .f = get_df_col_count, "log") |> unlist(),
        Numeric = purrr::map(
          .x = ds_list,
          .f = get_df_col_count, "num") |> unlist(),
        Character = purrr::map(
          .x = ds_list,
          .f = get_df_col_count, "chr") |> unlist(),
        Factor = purrr::map(
          .x = ds_list,
          .f = get_df_col_count, "fct") |> unlist(),
        List = purrr::map(
          .x = ds_list,
          .f = get_df_col_count, "lst") |> unlist())

    }

  } else {

    # data_results does not contains data objects
    return(data_results)

  }

}
