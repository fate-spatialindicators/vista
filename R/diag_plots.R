#' diagnostic_plots
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param X Character string containing the name of X variable
#' @param Y Character string containing the name of Y variable
#' @param time Character string containing the name of time variable
#' @param pred Character string containing the name of prediction variable
#' @param resid Character string containing the name of residual
#' @param demean_time Boolean, whether or not to remove temporal means
#' (similar to fixed time effects). Defaults to TRUE
#' @return
#' A list of ggplot objects that can be manipulated further
#'
#' @export
#' @examples
#' \donttest{
#' set.seed(2021)
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$pred = predict(m)
#  d$resid = residuals(m)
#  # the default names match, with the exception of year -- so change it
#  plots <- diagnostic_plots(d, time="year")
#' }
diagnostic_plots <- function(df, X = "X", Y = "Y", time = "time",
                             pred = "pred", resid = "resid",
                             demean_time = TRUE) {

  plots = list(
    pred_space = pred_space(df, X = X, Y = Y, time = time,
                            pred = pred, demean_time = TRUE, by_time = FALSE),
    pred_space_bytime = pred_space(df, X = X, Y = Y, time = time,
                                   pred = pred, demean_time = TRUE, by_time = TRUE),
    pred_time = pred_time(df, time = time, pred = pred),
    resid_space = resid_space(df, X = X, Y = Y, time = time,
                              resid = resid, by_time = FALSE),
    resid_space_bytime = resid_space(df, X = X, Y = Y, time = time,
                                     resid = resid, by_time = TRUE),
    resid_time = resid_time(df, time = time, resid = resid),
    sd_resid_time = sd_resid_time(df, time = time, resid = resid),
    sd_resid_space = sd_resid_space(df, X = X, Y = Y, time = time,
                                    resid = resid),
    qq = qq(df, time = time, resid = resid, by_time = FALSE),
    qq_time = qq(df, time = time, resid = resid, by_time = TRUE),
    qq_space = qq_space(df, X = X, Y = Y, time = time, resid = resid),
    pred_resid = pred_resid(df, time = time, pred = pred,
                            resid = resid, by_time = TRUE),
    moran_pred = moran_ts(df, time=time, response=pred),
    moran_resid = moran_ts(df, time=time, response=resid))

  return(plots)
}



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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#'
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$pred = predict(m)
#' pred_space(df = d, time="year")
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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#'
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$pred = predict(m)
#' # the default names match, with the exception of year -- so change it
#' pred_time(d, time="year")
#' }
pred_time <- function(df, time = "time", pred = "pred") {

  # coerce df to dataframe
  df <- as.data.frame(df)
  df[[time]] = as.factor(df[[time]])
  g <- ggplot(df, aes_string(time, pred, col = pred)) +
    geom_point(alpha = 0.3, position = position_dodge2(0.5)) +
    theme_bw() +
    xlab("") +
    ylab("") +
    ggtitle("Predictions across time steps")
  return(g)
}

#' resid_space
#'
#' @param df Dataframe, containing locations, time, and predictions
#' @param X Character string containing the name of X variable
#' @param Y Character string containing the name of Y variable
#' @param time Character string containing the name of time variable
#' @param resid Character string containing the name of residual variable
#' @param by_time Whether to facet by time (default = TRUE) or not
#'
#' @return
#' A ggplot object that can be manipulated further
#'
#' @import ggplot2
#' @importFrom stats as.formula
#' @export
#' @examples
#' \donttest{
#' set.seed(2021)
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$resid = resid(m)
#' # the default names match, with the exception of year -- so change it
#' resid_space(d, time="year")
#' }
resid_space <- function(df, X = "X", Y = "Y", time = "time", resid = "resid",
                        by_time = TRUE) {

  # coerce df to dataframe
  df <- as.data.frame(df)

  g <- ggplot(df, aes_string(X, Y, col = resid)) +
    geom_point(alpha = 0.5) +
    theme_bw() +
    xlab("") +
    ylab("") +
    scale_color_gradient2() +
    ggtitle("Residuals across time steps")
  if (by_time == TRUE) {
    g <- g + facet_wrap(as.formula(paste("~", time)))
  }
  return(g)
}

#' resid_time
#'
#' @param df Dataframe, containing locations, time, and predictions
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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$resid = resid(m)
#' # the default names match, with the exception of year -- so change it
#' resid_time(d, time="year")
#' }
resid_time <- function(df, time = "time", resid = "resid") {

  # coerce df to dataframe
  df <- as.data.frame(df)
  df[[time]] = as.factor(df[[time]])
  g <- ggplot(df, aes_string(time, resid, col = resid)) +
    geom_point(alpha = 0.3, position = position_dodge2(0.5)) +
    theme_bw() +
    xlab("") +
    ylab("") +
    ggtitle("Residuals across time steps")
  return(g)
}

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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$resid = resid(m)
#' # the default names match, with the exception of year -- so change it
#' sd_resid_time(d, time="year")
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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$resid = resid(m)
#' # the default names match, with the exception of year -- so change it
#' sd_resid_space(d, time="year")
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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$resid = resid(m)
#' # the default names match, with the exception of year -- so change it
#' qq(d, time="year")
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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$resid = resid(m)
#' # the default names match, with the exception of year -- so change it
#' qq_space(d, time="year")
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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$resid = resid(m)
#' # the default names match, with the exception of year -- so change it
#' pred_resid(d, time="year")
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
    g <- g + facet_wrap(as.formula(paste("~", time)), scales="free") +
      ggtitle("Predicted v residual")
  }
  return(g)
}

#' moran
#'
#' @param x numeric vector of responses
#' @param coords Matrix of coordinates corresponding to values in X, can also be data
#' frame that the distance matrix is generated from
#' @param scaled Boolean, whether to use scaled (standardized) values
#' @param alternative Character string containing the name of residual variable
#' @return
#' A list including the observed and expected statistic, standard error, and p-value
#'
#' @importFrom stats pnorm sd dist
#' @export
#' @examples
#' \donttest{
#' set.seed(2021)
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' moran_stat = moran(d$density, coords = d[,c("X","Y")])
#' }
moran = function (x, coords, scaled = FALSE, alternative = "two.sided")
{
  # code largely taken from ape::moran.I, but modified to take a list of coordinates to automate
  # could be extended to include other functions, but right now 1/dist is the weight
  weight <- as.matrix(1 / dist(coords,diag=TRUE,upper=TRUE))
  diag(weight) <- 0
  n <- length(x)

  ei <- -1/(n - 1)

  ROWSUM <- rowSums(weight)
  ROWSUM[ROWSUM == 0] <- 1
  weight <- weight/ROWSUM
  s <- sum(weight)
  m <- mean(x)
  y <- x - m
  cv <- sum(weight * y %o% y)
  v <- sum(y^2)
  obs <- (n/s) * (cv/v)
  if (scaled) {
    i.max <- (n/s) * (sd(rowSums(weight) * y)/sqrt(v/(n - 1)))
    obs <- obs/i.max
  }
  S1 <- 0.5 * sum((weight + t(weight))^2)
  S2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)
  s.sq <- s^2
  k <- (sum(y^4)/n)/(v/n)^2
  sdi <- sqrt((n * ((n^2 - 3 * n + 3) * S1 - n * S2 + 3 * s.sq) -
                 k * (n * (n - 1) * S1 - 2 * n * S2 + 6 * s.sq))/((n - 1) * (n - 2) * (n - 3) * s.sq) - 1/((n - 1)^2))
  alternative <- match.arg(alternative, c("two.sided", "less",
                                          "greater"))
  pv <- pnorm(obs, mean = ei, sd = sdi)
  if (alternative == "two.sided")
    pv <- if (obs <= ei)
      2 * pv
  else 2 * (1 - pv)
  if (alternative == "greater")
    pv <- 1 - pv
  list(observed = obs, expected = ei, sd = sdi, p.value = pv)
}


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
#' d <- data.frame(X=runif(1000), Y = runif(1000),
#' year = sample(1:10,size=1000,replace=TRUE))
#' d$density = rnorm(0.01*d$X -0.001*d$X*d$X + d$Y*0.02 - 0.005*d$Y*d$Y,0,0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X,Y),data=d)
#' d$resid = resid(m)
#' d$pred = predict(m)
#' # the default names match, with the exception of year -- so change it
#' moran_ts(d, time="year", response="pred")
#' moran_ts(d, time="year", response="resid")
#' }
moran_ts <- function(df, time = "time", X = "X", Y = "Y", response="pred",
                     scaled = FALSE, alpha=0.05) {
  lo <- hi <- obs <- NULL
  # coerce df to dataframe
  df <- as.data.frame(df)

  moran_df = data.frame("time"=as.numeric(unique(df[[time]])),
                        "mean" = NA, "lo"=NA, "hi"=NA, "p_val"=NA)
  for(i in 1:nrow(moran_df)) {
    indx = which(df[[time]] == moran_df$time[i])
    moran_stat =  moran(x = as.numeric(df[indx,response]), coords = cbind(df[indx,X], df[indx,Y]),
                        scaled=scaled)
    moran_df$mean[i] = moran_stat$expected
    moran_df$lo[i] = moran_stat$expected - 1.96*moran_stat$sd
    moran_df$hi[i] = moran_stat$expected + 1.96*moran_stat$sd
    moran_df$obs[i] = moran_stat$observed
    moran_df$p_val[i] = moran_stat$p.value
  }

  col = rep("black",nrow(moran_df))
  col[which(moran_df$p_val < alpha)] = "red"

  g <- ggplot(moran_df, aes(time, mean)) +
    geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.4) +
    theme_bw() +
    geom_point(data = moran_df, aes(time, obs), col=col) +
    xlab("") +
    xlab("") +
    ylab("Moran-I statistic")

  return(g)
}
