#' resid_space
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param X Character string containing the name of X variable
#' @param Y Character string containing the name of Y variable
#' @param time Character string containing the name of time variable
#' @param resid Character string containing the name of residual variable
#' @param by_time Whether to facet by time (default = TRUE) or not
#' @param outlier_sd Threshold for how residuals are classified as outliers; defaults to 3 
#' (e.g. a residual value more than 3 SDs is classified as an outlier)
#' @return
#' A ggplot object that can be manipulated further
#'
#' @import ggplot2
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
#' resid_space(d, time = "year")
#' }
resid_space <- function(df, X = "X", Y = "Y", time = "time", resid = "resid",
                        by_time = TRUE, outlier_sd = 3) {
  
  # coerce df to dataframe
  df <- as.data.frame(df)
  
  df$outlier = ifelse(abs(df[[resid]]) > outlier_sd * sd(df[[resid]]), 1, 0)
  g <- ggplot(df[which(df$outlier==0),], aes_string(X, Y, col = resid)) +
    geom_point(alpha = 0.5) +
    theme_bw() +
    xlab("") +
    ylab("") +
    scale_color_gradient2() +
    ggtitle("Residuals across time steps") + 
    geom_point(data = df[which(df$outlier==1),], shape=8)
  if (by_time == TRUE) {
    g <- g + facet_wrap(as.formula(paste("~", time)))
  }
  return(g)
}

