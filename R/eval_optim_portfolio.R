#' Bind log-return series to matrix
#'
#' @param log_ret_dt list of data.tables each with \code{log_ret}.
#' @return Numeric matrix (columns = strategies).
#' @keywords internal
#' @noRd
.log_ret_dt_to_mat <- function(log_ret_dt) {
  do.call(cbind, lapply(log_ret_dt, function(dt) dt$log_ret)) # need to be converted to ann_ret here
}

#' Covariance matrix from backtest results
#'
#' @param bt_res_list data.table of backtest rows with list-col \code{log_ret_dt}.
#' @return Numeric covariance matrix of log returns.
#' @keywords internal
#' @noRd
.bt_res_list_to_ret_cov_mat <- function(bt_res_list) {
  log_ret_mat <- .log_ret_dt_to_mat(bt_res_list$log_ret_dt)
  colnames(log_ret_mat) <- sprintf("%s_%s", bt_res_list$asset_name, bt_res_list$strat_label)
  cov(log_ret_mat, use = "pairwise.complete.obs")
}

#' Correlation matrix from backtest results
#'
#' @param bt_res_list data.table of backtest rows with list-col \code{log_ret_dt}.
#' @return Numeric correlation matrix of log returns.
#' @keywords internal
#' @noRd
.bt_res_list_to_ret_cor_mat <- function(bt_res_list) {
  log_ret_mat <- .log_ret_dt_to_mat(bt_res_list$log_ret_dt)
  colnames(log_ret_mat) <- sprintf("%s_%s", bt_res_list$asset_name, bt_res_list$strat_label)
  cor(log_ret_mat, use = "pairwise.complete.obs")
}

#' Minimum-variance optimizer
#'
#' @param ann_ret numeric vector of annualized returns.
#' @param cov_mat numeric covariance matrix (annualized).
#' @param target_return numeric annual target return.
#' @param long_only logical; nonnegative weights if \code{TRUE}.
#' @param equal_target logical; enforce exact target return if \code{TRUE}.
#' @param ridge numeric ridge added to the diagonal.
#' @return List with \code{weights}, \code{exp_return}, \code{vol_annual}, \code{status}.
#' @keywords internal
#' @noRd
.do_min_risk_optim <- function(ann_ret, cov_mat, target_return = 0.10,
                           long_only = TRUE, equal_target = FALSE, ridge = 1e-8) {
  stopifnot(is.numeric(ann_ret), is.matrix(cov_mat))
  n <- length(ann_ret); stopifnot(n == ncol(cov_mat), ncol(cov_mat) == nrow(cov_mat))

  # make PD and symmetric; scale by 2 for quadprog's 1/2 x^T D x
  D <- (cov_mat + t(cov_mat)) / 2
  D <- D + diag(ridge, n)
  D <- 2 * D

  d <- rep(0, n)  # pure variance minimization

  # constraints A^T x >= b
  A_budget <- rep(1, n)
  A_ret    <- ann_ret

  Amat <- cbind(A_budget)           # sum(weights) = 1 will be equality via meq
  bvec <- c(1)

  if (equal_target) {
    # exact target return: treat as equality (brittle but sometimes desired)
    Amat <- cbind(Amat, A_ret)
    bvec <- c(bvec, target_return)
    meq  <- 2
  } else {
    # return >= target (more practical/robust)
    Amat <- cbind(Amat, A_ret)
    bvec <- c(bvec, target_return)
    meq  <- 1
  }

  if (long_only) {
    Amat <- cbind(Amat, diag(n))    # x_i >= 0
    bvec <- c(bvec, rep(0, n))
  }

  res <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = Amat, bvec = bvec, meq = meq)

  w <- res$solution
  list(
    weights = w,
    exp_return = sum(w * ann_ret),
    vol_annual = sqrt(drop(t(w) %*% cov_mat %*% w)),
    status = res
  )
}

#' Compute optimal strategy weights
#'
#' @param bt_res_list data.table of backtest rows (from \code{eval_strat_performance()}).
#' @param target_return numeric annual target return (e.g., 0.10).
#' @return List with \code{optimal_weights} (named numeric) and \code{optimal_weights_table} (data.frame of nonzero weights).
#' @export
get_optimal_weights <- function(bt_res_list, target_return = 0.10) {
  cov_mat <- .bt_res_list_to_ret_cov_mat(bt_res_list)
  weights <- round(.do_min_risk_optim(bt_res_list$annual_return, cov_mat, target_return = target_return)$weights, 2)
  names(weights) <- dimnames(cov_mat)[[1]]
  non_zero_weights <- weights[weights != 0]
  df <- data.frame(
    name  = names(non_zero_weights),
    value = non_zero_weights
  )
  df <- df[order(-df$value), ] 
  list(
    optimal_weights = weights,
    optimal_weights_table = df
  )
}

#' Evaluate optimized portfolio performance
#'
#' @param bt_res_list data.table of backtest rows (with list-col \code{log_ret_dt}).
#' @param target_return numeric annual target return.
#' @return Invisibly: single-row data.table (portfolio backtest) with metrics and list-col \code{log_ret_dt}.
#' @export
eval_portfolio_performance <- function(bt_res_list, target_return) {
  weight_res <- get_optimal_weights(bt_res_list, target_return = target_return)
  
  datetime <- bt_res_list$log_ret_dt[1][[1]]$datetime
  log_ret_p <- .log_ret_dt_to_mat(bt_res_list$log_ret_dt) %*% weight_res$optimal_weights
  
  tbl <- weight_res$optimal_weights_table[1:3, ]
  
  port_res <- .log_ret_to_bt_res(
    datetime, log_ret_p, 
    asset_name = 'Optimized Portfolio', 
    bg_time = as.POSIXct(NA), ed_time = as.POSIXct(NA), 
    rf_rate = 0, 
    strat_name = 'Minimizing sd', 
    strat_par=list(Target_Ret = target_return), 
    strat_label = paste(sprintf("%s: %.2f", tbl$name, tbl$value), collapse = "; ")
  )
  
  return(invisible(port_res))
}

