#' Point plot (scatter-plot)
#'
#' @param df input dataset (tibble or data.frame)
#' @param x_var x variable (supplied to `ggplot2::aes(x = )`)
#' @param y_var y variable (supplied to `ggplot2::aes(y = )`)
#' @param ... other arguments passed to `ggplot2::geom_point()`, outside of
#' `ggplot2::aes()`
#'
#' @return A `ggplot2` plot object
#' @export gg_scatter
#'
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom ggplot2 labs theme_minimal theme
#' @importFrom stringr str_replace_all
#' @importFrom snakecase to_title_case
#'
#' @examples
#' require(dplyr)
#' starwars <- dplyr::starwars
#' gg_scatter(
#'   df = starwars,
#'   x_var = "height",
#'   y_var = "mass",
#'   alpha = 1 / 3,
#'   color = "#000000",
#'   size = 2
#' )
gg_scatter <- function(df, x_var, y_var, ...) {
  base <- gg_base(df = df, x_var = x_var, y_var = y_var)

  base +
    ggplot2::geom_point(...) +

    ggplot2::labs(
      title = gg_x_y_title(x = x_var, y = y_var),
      x = stringr::str_replace_all(
        snakecase::to_title_case(x_var), "_", " "
      ),
      y = stringr::str_replace_all(
        snakecase::to_title_case(y_var), "_", " "
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
