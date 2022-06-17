#' qq_space
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param X Character string containing the name of X variable
#' @param Y Character string containing the name of Y variable
#' @param time Character string containing the name of time variable
#' @param resid Character string containing the name of residual variable
#'
#' @return
#' A ggplot object that can be manipulated further
#'
#' @import ggplot2
#' @importFrom stats qqnorm
#' @importFrom stats as.formula
#' @export
#' @examples
#' \donttest{
#' set.seed(2021)
#' d <- data.frame(
#'   X = runif(1000), Y = runif(1000),
#'   year = sample(1:10, size = 1000, replace = TRUE)
#' )
#' d$density <- rnorm(0.01 * d$X - 0.001 * d$X * d$X + d$Y * 0.02 - 0.005 * d$Y * d$Y, 0, 0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X, Y), data = d)
#' d$resid <- resid(m)
#' # the default names match, with the exception of year -- so change it
#' qq_space(d, time = "year")
#' }
qq_space <- function(df, X = "X", Y = "Y", time = "time", resid = "resid") {
  
  # coerce df to dataframe
  df <- as.data.frame(df)
  
  qq <- stats::qqnorm(df[[resid]], plot.it = FALSE)
  df$qq <- qq$y - qq$x
  g <- ggplot(df, aes_string(X, Y, col = "qq")) +
    geom_point(alpha = 0.5) +
    facet_wrap(as.formula(paste("~", time))) +
    theme_bw() +
    xlab("") +
    ylab("") +
    scale_color_gradient2() +
    ggtitle("Sample quantile - theoretical quantile")
  return(g)
}


