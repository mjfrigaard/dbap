#' Colored point plot (scatter-plot)
#'
#' @param df input dataset (tibble or data.frame)
#' @param x_var x variable (supplied to `ggplot2::aes(x = )`)
#' @param y_var y variable (supplied to `ggplot2::aes(y = )`)
#' @param col_var color variable (supplied to `ggplot2::geom_point(ggplot2::aes(color = ))`)
#' @param ... other arguments passed to `ggplot2::geom_point()`, outside of `ggplot2::aes()`
#'
#' @return A `ggplot2` plot object
#' @export gg_color_scatter
#'
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom ggplot2 labs theme_minimal theme
#' @importFrom stringr str_replace_all
#' @importFrom snakecase to_title_case
#'
#' @examples
#' require(ggplot2)
#' diamonds <- ggplot2::diamonds
#' mini_dmnds <- diamonds[sample(nrow(diamonds), size = 1000), ]
#' gg_color_scatter(
#'   df = mini_dmnds,
#'   x_var = "carat",
#'   y_var = "price",
#'   col_var = "cut",
#'   alpha = 1 / 3,
#'   size = 2
#' )
gg_color_scatter <- function(df, x_var, y_var, col_var, ...) {
  base <- gg_base(df = df, x_var = x_var, y_var = y_var)

  base +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[col_var]]), ...
    ) +

    ggplot2::labs(
      title = gg_x_y_color_title(x = x_var, y = y_var, color = col_var),
      x = stringr::str_replace_all(
        snakecase::to_title_case(x_var), "_", " "
      ),
      y = stringr::str_replace_all(
        snakecase::to_title_case(y_var), "_", " "
      ),
      color = stringr::str_replace_all(
        snakecase::to_title_case(col_var), "_", " "
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
