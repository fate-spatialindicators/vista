#' sd_resid_space
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
#' sd_resid_space(d, time = "year")
#' }
sd_resid_space <- function(df, X = "X", Y = "Y", time = "time", resid = "resid") {
  
  # coerce df to dataframe
  df <- as.data.frame(df)
  
  # df here is a dataframe, where sites may or may not be replicated
  # across time steps
  df$unique_site <- paste(df[[X]], df[[Y]], df[[time]])
  n_site <- length(unique(df$unique_site))
  df_new <- data.frame(site = unique(df$unique_site), resid = 0)
  df_new$X <- as.numeric(unlist(lapply(
    strsplit(df$unique_site, " "),
    getElement, 1
  )))
  df_new$Y <- as.numeric(unlist(lapply(
    strsplit(df$unique_site, " "),
    getElement, 2
  )))
  for (i in 1:nrow(df_new)) {
    # find matches and take average across years
    orig_site <- which(df$unique_site == df_new$site[i])
    df_new$resid[i] <- sd(df[orig_site, resid], na.rm = T)
  }
  
  g <- ggplot(df_new, aes_string(X, Y, col = resid)) +
    geom_point(alpha = 0.5) +
    # scale_color_gradient2() +
    theme_bw() +
    xlab("") +
    ylab("") +
    labs(color = "Residual mean") +
    scale_color_gradient2() +
    ggtitle("Mean residuals across time steps")
  return(g)
}

