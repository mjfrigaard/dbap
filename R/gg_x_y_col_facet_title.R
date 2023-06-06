#' Make x, y, color plot title
#'
#' @param x x variable
#' @param y y variable
#' @param color color variable
#'
#' @return String for plot title
#' @export gg_x_y_col_facet_title
#'
#' @importFrom glue glue
#' @importFrom stringr str_replace_all
#' @importFrom snakecase to_title_case
#'
#' @examples
#' gg_x_y_col_facet_title(
#'   x = "height",
#'   y = "mass",
#'   color = "hair_color"
#' )
gg_x_y_col_facet_title <- function(x, y, color, facets) {
  x_chr <- stringr::str_replace_all(
    snakecase::to_title_case(x), "_", " "
  )
  y_chr <- stringr::str_replace_all(
    snakecase::to_title_case(y), "_", " "
  )
  color_chr <- stringr::str_replace_all(
    snakecase::to_title_case(color), "_", " "
  )
  facet_chr <- stringr::str_replace_all(
    snakecase::to_title_case(facets), "_", " "
  )
  glue::glue("{x_chr} vs. {y_chr} by {color_chr} & {facet_chr}")
}
