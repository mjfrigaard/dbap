#' Get all packages on search list
#'
#' @return All items from `search()` with a `package:` prefix
#' @export get_search_list_pkgs
#'
#' @description
#' This function is meant to be used in combination with `check_df_in_pkg()`
#'
#' @seealso [check_df_in_pkg()]
#'
#' @importFrom purrr set_names
#'
#' @examples
#' get_search_list_pkgs()
#'
get_search_list_pkgs <- function() {
  all_srch_lst <- search()
  all_pkgs <- grep(pattern = "package:", x = all_srch_lst, value = TRUE)
  pkgs <- gsub(pattern = ".*:|.GlobalEnv|datasets",
      replacement = "",
      x = all_pkgs)
  pkgs_chr <- pkgs[nzchar(pkgs)]
  pkg_nms <- purrr::set_names(pkgs_chr)
  return(pkg_nms)
}

#' Get names of data.frames from package
#'
#' @param pkg
#'
#' @return named vector of data.frames in package
#' @export get_pkg_df_names
#'
#' @examples
#' get_pkg_df_names(pkg = "base")
#' get_pkg_df_names(pkg = "datasets")
get_pkg_df_names <- function(pkg) {
  pkg_pos <- paste0("package:", pkg)
  pkg_nms <- ls(pkg_pos)
  data <- lapply(pkg_nms, get, pkg_pos)
  df_names <- pkg_nms[vapply(data, is.data.frame, logical(1))]
  if (length(df_names) > 1) {
    return(df_names)
  }
  return(NULL)
}


#' Check if package contains data.frame
#'
#' @param pkg name of package (a character vector)
#'
#' @return logical (`TRUE` = has data.frame, `FALSE` = no data.frame)
#' @export check_df_in_pkg
#'
#' @description
#' Returns `TRUE` if package has `data.frame`. If package is not installed,
#' install with `install.packages(dependencies = TRUE)`.
#'
#' 1. Check if the package is installed and load it
#'
#' 2. Retrieve the objects in the package
#'
#' 3. Use `purrr::map_lgl()` to apply `is.data.frame()` to each object in the
#'  package. `map_lgl()` returns a logical vector with the same length as
#'  the retrieved package objects.
#'
#'
#' @seealso [check_inst_pkg()]
#'
#' @importFrom purrr map_lgl
#'
#' @examples
#' check_df_in_pkg("dplyr")
#' check_df_in_pkg("stringr")
check_df_in_pkg <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    check_pkg(pkg = pkg)
  }
  pkg_obj <- ls(paste("package", pkg, sep = ":"))
  is_df <- purrr::map_lgl(.x = pkg_obj,
    ~ is.data.frame(get(x = .x,
      envir = as.environment(
        paste("package", pkg, sep = ":")
      ))))
  return(any(is_df))
}

#' Get packages with data.frames
#'
#' @return named vector of packages with `data.frame` objects
#' @export get_pkgs_with_dfs
#'
#' @importFrom purrr map_vec
#'
#' @examples
#' get_pkgs_with_dfs()
get_pkgs_with_dfs <- function() {
  all_pkgs <- get_search_list_pkgs()
  df_pkg_set <- purrr::map_vec(.x = all_pkgs, check_df_in_pkg)
  df_pkgs <- all_pkgs[df_pkg_set]
  return(df_pkgs)
}


#' Get package datasets metadata
#'
#' @param package name of package (a character vector)
#' @param allClass logical (include all classes of data?)
#' @param incPackage logical (include package name in result?)
#' @param maxTitle maximum length of dataset title
#'
#' @description
#' This is a variation on the `vcdExtra::datasets()` function.
#' Read more here:
#' https://github.com/friendly/vcdExtra/blob/master/R/datasets.R
#'
#'
#' @return `data.frame` with 6 variables (`dataset`, `title`, `dimensions`,
#' `obs`, `vars`, `display_title`)
#'
#' @export get_pkg_datameta
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate
#' @importFrom tidyr separate
#'
#' @examples
#' require(tidyr)
#' get_pkg_datameta("tidyr")
get_pkg_datameta <- function(package, allClass = FALSE,
                     incPackage = length(package) > 1,
                     maxTitle = NULL) {
  # make sure requested packages are available and loaded
  for (i in seq_along(package)) {
    if (!isNamespaceLoaded(package[i])) {
      if (requireNamespace(package[i], quietly = TRUE)) {
        cat(paste("Loading package:", package[i], "\n"))
      } else {
        stop(paste("Package", package[i], "is not available"))
      }
    }
  }
  dsitems <- data(package = package)$results
  wanted <- c("Package", "Item", "Title")
  ds <- as.data.frame(dsitems[, wanted], stringsAsFactors = FALSE)

  getData <- function(x, pkg) {
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

  getDim <- function(i) {
    data <- getData(ds$Item[i], ds$Package[i])
    if (is.null(dim(data))) length(data) else paste(dim(data), collapse = "x")
  }
  getClass <- function(i) {
    data <- getData(ds$Item[i], ds$Package[i])
    cl <- class(data)
    if (length(cl) > 1 && !allClass) cl[length(cl)] else cl
  }

  ds$dim <- unlist(lapply(seq_len(nrow(ds)), getDim))

  ds$class <- unlist(lapply(seq_len(nrow(ds)), getClass))
  if (!is.null(maxTitle)) ds$Title <- substr(ds$Title, 1, maxTitle)
  if (incPackage) {
    ds[c("Package", "Item", "class", "dim", "Title")]
  } else {
    ds[c("Item", "class", "dim", "Title")]
  }
  # named cols
  ds_cols <- dplyr::select(
    .data = ds,
    package = Package,
    dataset = Item,
    title = Title,
    dimensions = dim
  )
  # observations and variables
  ds_obs_vars <- tidyr::separate(
    data = ds_cols,
    col = dimensions,
    into = c("obs", "vars"),
    sep = "x",
    remove = TRUE
  )
  # tibble
  tbl_out <- tibble::as_tibble(ds_obs_vars)
  return(tbl_out)
}

# library(Lahman)
# dataset_list <- pkg_data(package = "Lahman")
# dataset_list
# pkg_data_metadata <- tibble::as_tibble(datasets(package = "Lahman"))
# pkg_data_metadata
