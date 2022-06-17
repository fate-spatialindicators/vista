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
#' d <- data.frame(
#'   X = runif(1000), Y = runif(1000),
#'   year = sample(1:10, size = 1000, replace = TRUE)
#' )
#' d$density <- rnorm(0.01 * d$X - 0.001 * d$X * d$X + d$Y * 0.02 - 0.005 * d$Y * d$Y, 0, 0.1)
#' moran_stat <- moran(d$density, coords = d[, c("X", "Y")])
#' }
moran <- function(x, coords, scaled = FALSE, alternative = "two.sided") {
  # code largely taken from ape::moran.I, but modified to take a list of coordinates to automate
  # could be extended to include other functions, but right now 1/dist is the weight
  weight <- as.matrix(1 / dist(coords, diag = TRUE, upper = TRUE))
  diag(weight) <- 0
  n <- length(x)
  
  ei <- -1 / (n - 1)
  
  ROWSUM <- rowSums(weight)
  ROWSUM[ROWSUM == 0] <- 1
  weight <- weight / ROWSUM
  s <- sum(weight)
  m <- mean(x)
  y <- x - m
  cv <- sum(weight * y %o% y)
  v <- sum(y^2)
  obs <- (n / s) * (cv / v)
  if (scaled) {
    i.max <- (n / s) * (sd(rowSums(weight) * y) / sqrt(v / (n - 1)))
    obs <- obs / i.max
  }
  S1 <- 0.5 * sum((weight + t(weight))^2)
  S2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)
  s.sq <- s^2
  k <- (sum(y^4) / n) / (v / n)^2
  sdi <- sqrt((n * ((n^2 - 3 * n + 3) * S1 - n * S2 + 3 * s.sq) -
                 k * (n * (n - 1) * S1 - 2 * n * S2 + 6 * s.sq)) / ((n - 1) * (n - 2) * (n - 3) * s.sq) - 1 / ((n - 1)^2))
  alternative <- match.arg(alternative, c(
    "two.sided", "less",
    "greater"
  ))
  pv <- pnorm(obs, mean = ei, sd = sdi)
  if (alternative == "two.sided") {
    pv <- if (obs <= ei) {
      2 * pv
    } else {
      2 * (1 - pv)
    }
  }
  if (alternative == "greater") {
    pv <- 1 - pv
  }
  list(observed = obs, expected = ei, sd = sdi, p.value = pv)
}