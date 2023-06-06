#' Make x, y, color plot title
#'
#' @param x x variable
#' @param y y variable
#' @param color color variable
#'
#' @return String for plot title
#' @export gg_x_y_color_title
#'
#' @importFrom glue glue
#' @importFrom stringr str_replace_all
#' @importFrom snakecase to_title_case
#'
#' @examples
#' require(ggplot2)
#' diamonds <- ggplot2::diamonds
#' mini_dmnds <- diamonds[sample(nrow(diamonds), size = 1000), ]
#' base <- ggplot2::ggplot(data = mini_dmnds,
#'   mapping = ggplot2::aes(x = carat, y = price, color = cut)) +
#'    ggplot2::geom_point()
#' base +
#'  ggplot2::labs(title = gg_x_y_color_title(
#'       x = "carat",
#'       y = "price",
#'       color = "cut"))
gg_x_y_color_title <- function(x, y, color) {
  x_chr <- stringr::str_replace_all(
    snakecase::to_title_case(x), "_", " "
  )
  y_chr <- stringr::str_replace_all(
    snakecase::to_title_case(y), "_", " "
  )
  color_chr <- stringr::str_replace_all(
    snakecase::to_title_case(color), "_", " "
  )
  glue::glue("{x_chr} vs. {y_chr} by {color_chr}")
}
