#' moran_ts
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param time Character string containing the name of time variable
#' @param X Character string containing the name of X variable
#' @param Y Character string containing the name of Y variable
#' @param pred Character string containing the name of resonse variable to test
#' @param alpha Alpha level for significance test, defaults to 0.05
#' @param response The response to use for the Moran test. Name of variable in 'df',
#'  defaults to "pred"
#' @param scaled Boolean, whether to use scaled (standardized) values in Moran statistic
#' defaults to FALSE
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
#' d$pred <- predict(m)
#' # the default names match, with the exception of year -- so change it
#' moran_ts(d, time = "year", response = "pred")
#' moran_ts(d, time = "year", response = "resid")
#' }
moran_ts <- function(df, time = "time", X = "X", Y = "Y", response = "pred",
                     scaled = FALSE, alpha = 0.05) {
  lo <- hi <- obs <- NULL
  # coerce df to dataframe
  df <- as.data.frame(df)
  
  moran_df <- data.frame(
    "time" = as.numeric(unique(df[[time]])),
    "mean" = NA, "lo" = NA, "hi" = NA, "p_val" = NA
  )
  for (i in 1:nrow(moran_df)) {
    indx <- which(df[[time]] == moran_df$time[i])
    moran_stat <- moran(
      x = as.numeric(df[indx, response]), coords = cbind(df[indx, X], df[indx, Y]),
      scaled = scaled
    )
    moran_df$mean[i] <- moran_stat$expected
    moran_df$lo[i] <- moran_stat$expected - 1.96 * moran_stat$sd
    moran_df$hi[i] <- moran_stat$expected + 1.96 * moran_stat$sd
    moran_df$obs[i] <- moran_stat$observed
    moran_df$p_val[i] <- moran_stat$p.value
  }
  
  col <- rep("black", nrow(moran_df))
  col[which(moran_df$p_val < alpha)] <- "red"
  
  g <- ggplot(moran_df, aes(time, mean)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.4) +
    theme_bw() +
    geom_point(data = moran_df, aes(time, obs), col = col) +
    xlab("") +
    xlab("") +
    ylab("Moran-I statistic")
  
  return(g)
}

