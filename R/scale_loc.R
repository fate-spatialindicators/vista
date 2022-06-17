#' scale_loc
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param time Character string containing the name of time variable
#' @param pred Character string containing the name of prediction variable
#' @param resid Character string containing the name of residual variable
#' @param by_time Whether to facet by time (default = TRUE) or not
#' @param add_smooth Whether to include a smoothed line via geom_smooth (default = TRUE)
#'
#' @return
#' A ggplot object that can be manipulated further
#'
#' @import ggplot2
#' @export
#' @examples
#' \donttest{
#' set.seed(2021)
#' d <- data.frame(
#'   X = runif(1000), Y = runif(1000),
#'   year = sample(1:10, size = 1000, replace = TRUE)
#' )
#' d$density <- rnorm(0.01 * d$X - 0.001 * d$X * d$X + d$Y * 0.02 - 0.005 * d$Y * d$Y, 0, 0.1)
#'
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X, Y), data = d)
#' d$pred <- predict(m)
#' d$resid <- resid(m)
#' # the default names match, with the exception of year -- so change it
#' scale_loc(d, time = "year")
#' }
scale_loc <- function(df, time = "time", pred = "pred", resid = "resid", by_time = TRUE, add_smooth = TRUE) {
  
  # coerce df to dataframe
  df <- as.data.frame(df)
  df[[time]] <- as.factor(df[[time]])
  
  # sqrt of absolute value of standardized residuals
  df[[resid]] <- sqrt(abs(df[[resid]] / sd(df[[resid]])))
  g <- ggplot(df, aes_string(pred, resid)) +
    geom_point(alpha = 0.3) +
    theme_bw() +
    xlab("Predicted") +
    ylab("Standardized residual")
  if (by_time == TRUE) {
    g <- g + facet_wrap(as.formula(paste("~", time)), scales = "free") +
      ggtitle("Scale - location plot")
  }
  if (add_smooth == TRUE) {
    g <- g + geom_smooth()
  }
  return(g)
}
