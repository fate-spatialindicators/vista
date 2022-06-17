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
#' d <- data.frame(
#'   X = runif(1000), Y = runif(1000),
#'   year = sample(1:10, size = 1000, replace = TRUE)
#' )
#' d$density <- rnorm(0.01 * d$X - 0.001 * d$X * d$X + d$Y * 0.02 - 0.005 * d$Y * d$Y, 0, 0.1)
#' m <- mgcv::gam(density ~ 0 + as.factor(year) + s(X, Y), data = d)
#' d$pred <- predict(m)
#' #  d$resid = residuals(m)
#' #  # the default names match, with the exception of year -- so change it
#' #  plots <- diagnostic_plots(d, time="year")
#' }
diagnostic_plots <- function(df, X = "X", Y = "Y", time = "time",
                             pred = "pred", resid = "resid",
                             demean_time = TRUE) {
  plots <- list(
    pred_space = pred_space(df,
      X = X, Y = Y, time = time,
      pred = pred, demean_time = TRUE, by_time = FALSE
    ),
    pred_space_bytime = pred_space(df,
      X = X, Y = Y, time = time,
      pred = pred, demean_time = TRUE, by_time = TRUE
    ),
    pred_time = pred_time(df, time = time, pred = pred),
    resid_space = resid_space(df,
      X = X, Y = Y, time = time,
      resid = resid, by_time = FALSE
    ),
    resid_space_bytime = resid_space(df,
      X = X, Y = Y, time = time,
      resid = resid, by_time = TRUE
    ),
    resid_time = resid_time(df, time = time, resid = resid),
    sd_resid_time = sd_resid_time(df, time = time, resid = resid),
    sd_resid_space = sd_resid_space(df,
      X = X, Y = Y, time = time,
      resid = resid
    ),
    qq = qq(df, time = time, resid = resid, by_time = FALSE),
    qq_time = qq(df, time = time, resid = resid, by_time = TRUE),
    qq_space = qq_space(df, X = X, Y = Y, time = time, resid = resid),
    pred_resid = pred_resid(df,
      time = time, pred = pred,
      resid = resid, by_time = TRUE
    ),
    moran_pred = moran_ts(df, time = time, response = pred),
    moran_resid = moran_ts(df, time = time, response = resid),
    scale_loc = scale_loc(df, time=time, resid=resid, pred=pred,by_time=FALSE),
    scale_loc_time = scale_loc(df, time=time, resid=resid, pred=pred,by_time=TRUE)
  )

  return(plots)
}



