#' sd_resid_time
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param time Character string containing the name of time variable
#' @param resid Character string containing the name of residual variable
#'
#' @return
#' A ggplot object that can be manipulated further
#'
#' @import ggplot2
#' @importFrom stats quantile sd
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
#' sd_resid_time(d, time = "year")
#' }
sd_resid_time <- function(df, time = "time", resid = "resid") {
  lo <- hi <- obs <- stddev <- NULL
  # coerce df to dataframe
  df <- as.data.frame(df)
  
  df_new <- data.frame(
    "time" = unique(df[[time]]),
    "stddev" = 0,
    "lo" = 0,
    "hi" = 0
  )
  for (i in 1:nrow(df_new)) {
    # bootstrap the residuals to get the se and uncertainty
    indx <- which(df[[time]] == df_new$time[i])
    resamples <- lapply(1:1000, function(i) sample(df[indx, resid], replace = T))
    sds <- sapply(resamples, sd)
    df_new$stddev[i] <- mean(sds)
    df_new$lo[i] <- quantile(sds, 0.025)
    df_new$hi[i] <- quantile(sds, 0.975)
  }
  
  g <- ggplot(df_new, aes(time, stddev)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.3) +
    geom_line() +
    geom_point(col = "black") +
    theme_bw() +
    xlab("") +
    ylab("Standard deviation of residuals") +
    ggtitle("Residual standard deviation, +/- 95% CIs")
  return(g)
}

