#' ggplot2 x, y plot title
#'
#' @param x x variable
#' @param y y variable
#' @param color color variable
#'
#' @return String for plot title
#' @export gg_x_y_title
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
#'   mapping = ggplot2::aes(x = carat, y = price)) +
#'    ggplot2::geom_point()
#' base +
#'  ggplot2::labs(title =
#'     gg_x_y_title(
#'       x = "height",
#'       y = "mass"))
gg_x_y_title <- function(x, y) {
  x_chr <- stringr::str_replace_all(
    snakecase::to_title_case(x), "_", " "
  )
  y_chr <- stringr::str_replace_all(
    snakecase::to_title_case(y), "_", " "
  )
  glue::glue("{x_chr} vs. {y_chr}")
}
