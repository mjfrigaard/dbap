#' Base plot (ggplot2)
#'
#' @param df input dataset (tibble or data.frame)
#' @param x_var x variable
#' @param y_var y variable
#'
#' @return plot object
#' @export gg_base
#'
#' @importFrom ggplot2 ggplot aes
#'
#' @examples
#' require(ggplot2)
#' diamonds <- ggplot2::diamonds
#' mini_dmnds <- diamonds[sample(nrow(diamonds), size = 1000), ]
#' gg_base(df = mini_dmnds, x_var = "carat", y_var = "price")
gg_base <- function(df, x_var, y_var) {
  ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
  )
}
