#' Validate simulation length arguments
#'
#' @param n Number of kept observations.
#' @param burn Number of burn-in observations.
#'
#' @return Invisibly returns `TRUE`.
#' @keywords internal
#' @noRd
.sim_validate_length_args <- function(n, burn) {
  stopifnot(length(n) == 1L, is.numeric(n), is.finite(n), n >= 1)
  stopifnot(length(burn) == 1L, is.numeric(burn), is.finite(burn), burn >= 0)
  invisible(TRUE)
}

#' Format a simulated time series as a research-ready table
#'
#' @param value Numeric vector of kept observations.
#' @param innovation Numeric vector of kept innovations.
#' @param sigma2 Optional numeric vector of conditional variances.
#'
#' @return `data.table`.
#' @keywords internal
#' @noRd
.sim_as_dt <- function(value, innovation, sigma2 = NULL) {
  dt <- data.table::data.table(
    t = seq_along(value),
    value = as.numeric(value),
    innovation = as.numeric(innovation)
  )

  if (!is.null(sigma2)) {
    dt[, sigma2 := as.numeric(sigma2)]
  }

  dt[]
}

#' Simulate an AR(1) data-generating process
#'
#' @param n Number of observations returned after burn-in.
#' @param a1 AR(1) coefficient.
#' @param y_lag1 Initial lagged value.
#' @param burn Number of burn-in observations discarded before returning the sample.
#' @param sigma Innovation standard deviation.
#' @param drift Constant drift term added each step.
#' @param trend_slope Linear trend slope applied to the running time index.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return `data.table` with columns `t`, `value`, and `innovation`.
#' @export
sim_ar1_dgp <- function(n, a1, y_lag1 = 0, burn = 50, sigma = 1, drift = 0, trend_slope = 0, seed = NULL) {
  .sim_validate_length_args(n, burn)
  stopifnot(length(a1) == 1L, is.numeric(a1), is.finite(a1))
  stopifnot(length(sigma) == 1L, is.numeric(sigma), is.finite(sigma), sigma > 0)

  if (!is.null(seed)) set.seed(seed)

  total_n <- as.integer(n + burn)
  innovation <- stats::rnorm(total_n, sd = sigma)
  value <- numeric(total_n)

  for (i in seq_len(total_n)) {
    value[i] <- a1 * y_lag1 + innovation[i] + drift + trend_slope * i
    y_lag1 <- value[i]
  }

  keep <- seq.int(burn + 1L, total_n)
  .sim_as_dt(value = value[keep], innovation = innovation[keep])
}

#' Simulate an AR(2) data-generating process
#'
#' @inheritParams sim_ar1_dgp
#' @param a2 AR(2) coefficient.
#' @param y_lag2 Second initial lagged value.
#'
#' @return `data.table` with columns `t`, `value`, and `innovation`.
#' @export
sim_ar2_dgp <- function(n, a1, a2, y_lag1 = 0, y_lag2 = 0, burn = 50, sigma = 1, drift = 0, trend_slope = 0, seed = NULL) {
  .sim_validate_length_args(n, burn)
  stopifnot(all(is.numeric(c(a1, a2))), all(is.finite(c(a1, a2))))
  stopifnot(length(sigma) == 1L, is.numeric(sigma), is.finite(sigma), sigma > 0)

  if (!is.null(seed)) set.seed(seed)

  total_n <- as.integer(n + burn)
  innovation <- stats::rnorm(total_n, sd = sigma)
  value <- numeric(total_n)

  for (i in seq_len(total_n)) {
    value[i] <- a1 * y_lag1 + a2 * y_lag2 + innovation[i] + drift + trend_slope * i
    y_lag2 <- y_lag1
    y_lag1 <- value[i]
  }

  keep <- seq.int(burn + 1L, total_n)
  .sim_as_dt(value = value[keep], innovation = innovation[keep])
}

#' Simulate an AR(3) data-generating process
#'
#' @inheritParams sim_ar2_dgp
#' @param a3 AR(3) coefficient.
#' @param y_lag3 Third initial lagged value.
#'
#' @return `data.table` with columns `t`, `value`, and `innovation`.
#' @export
sim_ar3_dgp <- function(n, a1, a2, a3, y_lag1 = 0, y_lag2 = 0, y_lag3 = 0, burn = 50, sigma = 1, drift = 0, trend_slope = 0, seed = NULL) {
  .sim_validate_length_args(n, burn)
  stopifnot(all(is.numeric(c(a1, a2, a3))), all(is.finite(c(a1, a2, a3))))
  stopifnot(length(sigma) == 1L, is.numeric(sigma), is.finite(sigma), sigma > 0)

  if (!is.null(seed)) set.seed(seed)

  total_n <- as.integer(n + burn)
  innovation <- stats::rnorm(total_n, sd = sigma)
  value <- numeric(total_n)

  for (i in seq_len(total_n)) {
    value[i] <- a1 * y_lag1 + a2 * y_lag2 + a3 * y_lag3 + innovation[i] + drift + trend_slope * i
    y_lag3 <- y_lag2
    y_lag2 <- y_lag1
    y_lag1 <- value[i]
  }

  keep <- seq.int(burn + 1L, total_n)
  .sim_as_dt(value = value[keep], innovation = innovation[keep])
}

#' Simulate an MA(1) data-generating process
#'
#' @inheritParams sim_ar1_dgp
#' @param b1 MA(1) coefficient.
#' @param e_lag1 Initial lagged innovation.
#'
#' @return `data.table` with columns `t`, `value`, and `innovation`.
#' @export
sim_ma1_dgp <- function(n, b1, e_lag1 = 0, burn = 50, sigma = 1, seed = NULL) {
  .sim_validate_length_args(n, burn)
  stopifnot(length(b1) == 1L, is.numeric(b1), is.finite(b1))
  stopifnot(length(sigma) == 1L, is.numeric(sigma), is.finite(sigma), sigma > 0)

  if (!is.null(seed)) set.seed(seed)

  total_n <- as.integer(n + burn)
  innovation <- stats::rnorm(total_n, sd = sigma)
  value <- numeric(total_n)

  for (i in seq_len(total_n)) {
    value[i] <- innovation[i] + b1 * e_lag1
    e_lag1 <- innovation[i]
  }

  keep <- seq.int(burn + 1L, total_n)
  .sim_as_dt(value = value[keep], innovation = innovation[keep])
}

#' Simulate an ARCH(1) data-generating process
#'
#' @param n Number of observations returned after burn-in.
#' @param alpha0 Constant term in the conditional variance recursion.
#' @param alpha1 ARCH coefficient on the lagged squared innovation.
#' @param e_lag1_sq Initial lagged squared innovation.
#' @param burn Number of burn-in observations discarded before returning the sample.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return `data.table` with columns `t`, `value`, `innovation`, and `sigma2`.
#' @export
sim_arch1_dgp <- function(n, alpha0, alpha1, e_lag1_sq = 0, burn = 50, seed = NULL) {
  .sim_validate_length_args(n, burn)
  stopifnot(all(is.numeric(c(alpha0, alpha1))), all(is.finite(c(alpha0, alpha1))))
  stopifnot(alpha0 > 0, alpha1 >= 0)

  if (!is.null(seed)) set.seed(seed)

  total_n <- as.integer(n + burn)
  innovation <- numeric(total_n)
  sigma2 <- numeric(total_n)

  for (i in seq_len(total_n)) {
    sigma2[i] <- alpha0 + alpha1 * e_lag1_sq
    innovation[i] <- stats::rnorm(1L, sd = sqrt(sigma2[i]))
    e_lag1_sq <- innovation[i]^2
  }

  keep <- seq.int(burn + 1L, total_n)
  .sim_as_dt(value = innovation[keep], innovation = innovation[keep], sigma2 = sigma2[keep])
}

#' Simulate a GARCH(1,1) data-generating process
#'
#' @param n Number of observations returned after burn-in.
#' @param alpha0 Constant term in the conditional variance recursion.
#' @param alpha1 Coefficient on the lagged squared innovation.
#' @param beta1 Coefficient on the lagged conditional variance.
#' @param sigma2_0 Optional initial conditional variance. If `NULL`, the unconditional variance is used.
#' @param burn Number of burn-in observations discarded before returning the sample.
#' @param z_dist Innovation distribution: `"norm"` or `"stdt"`.
#' @param df Degrees of freedom for the standardized Student t innovations.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return `data.table` with columns `t`, `value`, `innovation`, and `sigma2`.
#' @export
sim_garch11_dgp <- function(n, alpha0, alpha1, beta1, sigma2_0 = NULL, burn = 50, z_dist = c("norm", "stdt"), df = 8, seed = NULL) {
  .sim_validate_length_args(n, burn)
  stopifnot(all(is.numeric(c(alpha0, alpha1, beta1))), all(is.finite(c(alpha0, alpha1, beta1))))
  stopifnot(alpha0 > 0, alpha1 >= 0, beta1 >= 0)
  z_dist <- match.arg(z_dist)

  if (z_dist == "stdt") stopifnot(length(df) == 1L, is.numeric(df), is.finite(df), df > 2)
  if (!is.null(seed)) set.seed(seed)

  total_n <- as.integer(n + burn)
  sigma2 <- numeric(total_n)
  innovation <- numeric(total_n)

  if (is.null(sigma2_0)) {
    denom <- 1 - alpha1 - beta1
    if (denom <= 0) stop("Need alpha1 + beta1 < 1 for finite unconditional variance.", call. = FALSE)
    sigma2_t <- alpha0 / denom
  } else {
    stopifnot(length(sigma2_0) == 1L, is.numeric(sigma2_0), is.finite(sigma2_0), sigma2_0 > 0)
    sigma2_t <- sigma2_0
  }

  draw_z <- function(n_draws) {
    if (identical(z_dist, "norm")) return(stats::rnorm(n_draws))
    z <- stats::rt(n_draws, df = df)
    z / sqrt(df / (df - 2))
  }

  for (i in seq_len(total_n)) {
    sigma2[i] <- sigma2_t
    z_t <- draw_z(1L)
    innovation[i] <- sqrt(sigma2_t) * z_t
    sigma2_t <- alpha0 + alpha1 * innovation[i]^2 + beta1 * sigma2_t
  }

  keep <- seq.int(burn + 1L, total_n)
  .sim_as_dt(value = innovation[keep], innovation = innovation[keep], sigma2 = sigma2[keep])
}
