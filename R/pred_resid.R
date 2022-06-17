#' pred_resid
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param time Character string containing the name of time variable
#' @param pred Character string containing the name of predicted variable
#' @param resid Character string containing the name of residual variable
#' @param by_time Whether to facet by time (default = TRUE) or not
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
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X, Y), data = d)
#' d$resid <- resid(m)
#' # the default names match, with the exception of year -- so change it
#' pred_resid(d, time = "year")
#' }
pred_resid <- function(df, time = "time", pred = "pred", resid = "resid", by_time = TRUE) {
  
  # coerce df to dataframe
  df <- as.data.frame(df)
  
  g <- ggplot(df, aes_string(pred, resid)) +
    geom_point(alpha = 0.5) +
    theme_bw() +
    xlab("") +
    ylab("") +
    scale_color_gradient2() +
    geom_smooth() +
    xlab("Predicted") +
    ylab("Residual")
  if (by_time == TRUE) {
    g <- g + facet_wrap(as.formula(paste("~", time)), scales = "free") +
      ggtitle("Predicted v residual")
  }
  return(g)
}




