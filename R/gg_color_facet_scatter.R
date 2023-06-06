#' Colored point plot (scatter-plot) with facets
#'
#' @param df input dataset (tibble or data.frame)
#' @param x_var x variable (supplied to `ggplot2::aes(x = )`)
#' @param y_var y variable (supplied to `ggplot2::aes(y = )`)
#' @param col_var color variable (supplied to `ggplot2::geom_point(ggplot2::aes(color = ))`)
#' @param facet_var facet variable (supplied to `ggplot2::geom_point(ggplot2::aes(color = ))`)
#' @param ... other arguments passed to (`ggplot2::facet_wrap(vars())`)
#'
#' @return A `ggplot2` plot object
#' @export gg_color_facet_scatter
#'
#' @importFrom ggplot2 ggplot aes vars facet_wrap geom_point labs
#' @importFrom rlang .data
#'
#' @examples
#' require(ggplot2)
#' diamonds <- ggplot2::diamonds
#' mini_dmnds <- diamonds[sample(nrow(diamonds), size = 1000), ]
#' gg_color_facet_scatter(
#'   df = mini_dmnds,
#'   x_var = "carat",
#'   y_var = "price",
#'   col_var = "cut",
#'   facet_var = "cut",
#'   alpha = 1 / 3,
#'   size = 2
#' )
#' # compare with
#' ggplot2::ggplot(
#'   data = mini_dmnds,
#'   mapping = ggplot2::aes(x = carat, y = price)
#' ) +
#'   ggplot2::geom_point(aes(color = cut, group = cut),
#'     size = 2, alpha = 1 / 3
#'   ) +
#'   ggplot2::facet_wrap(. ~ cut) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::theme(legend.position = "bottom")
#'
#' gg_color_facet_scatter(
#'   df = mini_dmnds,
#'   x_var = "carat",
#'   y_var = "price",
#'   col_var = "cut",
#'   facet_var = NULL,
#'   alpha = 1 / 3,
#'   size = 2
#' )
#' # compare with
#' ggplot2::ggplot(
#'   data = mini_dmnds,
#'   mapping = ggplot2::aes(x = carat, y = price)
#' ) +
#'   ggplot2::geom_point(aes(color = cut, group = cut),
#'     size = 2, alpha = 1 / 3
#'   ) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::theme(legend.position = "bottom")
#' gg_color_facet_scatter(
#'   df = mini_dmnds,
#'   x_var = "carat",
#'   y_var = "price",
#'   col_var = NULL,
#'   facet_var = NULL,
#'   alpha = 1 / 3,
#'   size = 2
#' )
#' # compare with
#' ggplot(
#'   data = mini_dmnds,
#'   mapping = ggplot2::aes(x = carat, y = price)) +
#'   ggplot2::geom_point(size = 2, alpha = 1 / 3) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::theme(legend.position = "bottom")
gg_color_facet_scatter <- function(
    df, x_var, y_var, col_var = NULL,
    facet_var = NULL, ...) {
  if (!is.null(col_var) & !is.null(facet_var)) {
    ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    ) +
      # points layer
      # add ... for alpha and size passed to points
      ggplot2::geom_point(
        ggplot2::aes(colour = .data[[col_var]], group = .data[[col_var]]), ...
      ) +
      # add facet layer
      ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]])) +
      # add labels
      ggplot2::labs(
        title = gg_x_y_col_facet_title(
          x = x_var, y = y_var,
          color = col_var, facets = facet_var
        ),
        x = stringr::str_replace_all(
          snakecase::to_title_case(x_var), "_", " "
        ),
        y = stringr::str_replace_all(
          snakecase::to_title_case(y_var), "_", " "
        ),
        color = stringr::str_replace_all(
          snakecase::to_title_case(col_var), "_", " "
        ),
        group = stringr::str_replace_all(
          snakecase::to_title_case(facet_var), "_", " "
        )
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
  } else if (!is.null(col_var) & is.null(facet_var)) {
    ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    ) +
      # add ... for alpha and size passed to points
      ggplot2::geom_point(
        ggplot2::aes(colour = .data[[col_var]], group = .data[[col_var]]), ...
      ) +
      # add labels
      ggplot2::labs(
        title = gg_x_y_color_title(
          x = x_var,
          y = y_var,
          color = col_var
        ),
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
  } else if (is.null(col_var) & is.null(facet_var)) {
    ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    ) +
      ggplot2::geom_point(...) +
      # add labels
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
  } else {
    ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    ) +
      # add labels
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
}
