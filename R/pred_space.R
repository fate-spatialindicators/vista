#'
#'
#' pred_space
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param X Character string containing the name of X variable
#' @param Y Character string containing the name of Y variable
#' @param time Character string containing the name of time variable
#' @param pred Character string containing the name of prediction variable
#' @param demean_time Boolean, whether or not to remove temporal means
#' (similar to fixed time effects). Defaults to TRUE
#' @param by_time Whether to facet by time (default = TRUE) or not
#' @return
#' A ggplot object that can be manipulated further
#'
#' @import ggplot2
#' @import viridis
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
#'
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X, Y), data = d)
#' d$pred <- predict(m)
#' pred_space(df = d, time = "year")
#' }
pred_space <- function(df, X = "X", Y = "Y", time = "time",
                       pred = "pred", demean_time = TRUE, by_time = TRUE) {
  
  # coerce df to dataframe
  df <- as.data.frame(df)
  
  if (demean_time == TRUE) {
    # remove year effects
    for (t in unique(df[[time]])) {
      indx <- which(df[[time]] == t)
      df[indx, pred] <- df[indx, pred] - mean(df[indx, pred], na.rm = T)
    }
  }
  
  g <- ggplot(df, aes_string(X, Y, col = pred)) +
    geom_point(alpha = 0.5) +
    theme_bw() +
    xlab("") +
    ylab("") +
    scale_color_viridis(end = 0.8) +
    ggtitle("Predictions across time steps")
  if (by_time == TRUE) {
    g <- g + facet_wrap(as.formula(paste("~", time)))
  }
  return(g)
}

