#' pred_time
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param time Character string containing the name of time variable
#' @param pred Character string containing the name of prediction variable
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
#' # the default names match, with the exception of year -- so change it
#' pred_time(d, time = "year")
#' }
pred_time <- function(df, time = "time", pred = "pred") {
  
  # coerce df to dataframe
  df <- as.data.frame(df)
  df[[time]] <- as.factor(df[[time]])
  g <- ggplot(df, aes_string(time, pred, col = pred)) +
    geom_point(alpha = 0.3, position = position_dodge2(0.5)) +
    theme_bw() +
    xlab("") +
    ylab("") +
    ggtitle("Predictions across time steps")
  return(g)
}

