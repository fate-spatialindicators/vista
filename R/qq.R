#' qq
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param time Character string containing the name of time variable
#' @param resid Character string containing the name of residual variable
#' @param by_time Whether to facet by time (default = TRUE) or not
#' @return
#' A ggplot object that can be manipulated further
#'
#' @import ggplot2
#' @importFrom qqplotr stat_qq_band
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
#' qq(d, time = "year")
#' }
qq <- function(df, time = "time", resid = "resid", by_time = TRUE) {
  
  # coerce df to dataframe
  df <- as.data.frame(df)
  
  g <- ggplot(df, aes(sample = resid)) +
    stat_qq_band(alpha = 0.3) +
    stat_qq_line() +
    stat_qq(alpha = 0.4) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    xlab("Theoretical") +
    ylab("Sample")
  if (by_time == TRUE) {
    g <- g + facet_wrap(as.formula(paste("~", time)))
  }
  return(g)
}


