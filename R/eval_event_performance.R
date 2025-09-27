#' Future returns over horizons
#'
#' @param DT data.table with at least \code{close}.
#' @param H integer vector of horizons (e.g., \code{1L:42L}).
#' @param use_log logical; log returns if \code{TRUE}.
#' @param matrix_mode logical; return matrix if \code{TRUE}, otherwise write columns back to \code{DT}.
#' @param mode one of \code{"cum"} (t→t+h) or \code{"step"} ((t+h-1)→(t+h)).
#' @return Invisibly: matrix (cols \code{h{H}}) if \code{matrix_mode}; otherwise \code{DT} with added columns.
#' @keywords internal
#' @noRd
.eval_calc_future_returns <- function(DT, H = 1L:42L, use_log = FALSE, matrix_mode = TRUE, mode = c("cum", "step")) {
  mode <- match.arg(mode)
  stopifnot(is.integer(H))
  close <- DT[["close"]]
  base  <- if (use_log) log(close) else close

  # Prices at t+h
  leads_list <- data.table::shift(base, n = H, type = "lead")
  F <- do.call(cbind, leads_list)

  if (mode == "cum") {
    # cumulative: t -> t+h
    B <- matrix(base, nrow = length(base), ncol = length(H))
    ret <- if (use_log) (F - B) else (F / B - 1)
  } else {
    # step (per-moment): (t+h-1) -> (t+h), with h=1 comparing to t
    prev_list <- lapply(H, function(h) if (h == 1L) base else data.table::shift(base, n = h - 1L, type = "lead"))
    Pprev <- do.call(cbind, prev_list)
    ret <- if (use_log) (F - Pprev) else (F / Pprev - 1)
  }

  if (matrix_mode) {
    dimnames(ret) <- list(NULL, paste0("h", H))
    return(invisible(ret))
  } else {
    # write columns back to DT
    nm <- if (mode == "cum") sprintf("fut_ret_%d", H) else sprintf("fut_step_ret_%d", H)
    ret_list <- lapply(seq_along(H), function(i) ret[, i])
    data.table::set(DT, j = nm, value = ret_list)
    return(invisible(DT))
  }
}

#' Evaluate event performance
#'
#' @param DT data.table with \code{datetime}, \code{close}; optionally \code{attributes(DT)$inst_id}.
#' @param event_col string; column in \code{DT} with 1L at event rows.
#' @param event_score_cols character vector of extra columns to carry per case.
#' @param H integer horizons (default \code{1L:42L}).
#' @param threshold numeric hit threshold (absolute return).
#' @return Invisibly: data.table (one row) with summary stats and list-cols \code{performance_by_cases}, \code{performance_by_horizons}.
#' @export
eval_event_performance <- function(DT, event_col, event_score_cols = character(0L), H = 1L:42L, threshold = 0.02) {
  
  datetime <- DT$datetime
  start_dt <- datetime[1L]
  end_dt <- datetime[length(datetime)]
  
  ret_cum_mat <- .eval_calc_future_returns(DT, H, use_log = FALSE, matrix_mode = TRUE, mode = 'cum')
  # ret_step_mat <- .eval_calc_future_returns(DT, H, use_log = TRUE, matrix_mode = TRUE, mode = 'step')
  
  event_rows <- which(DT[[event_col]] == 1L)
  n_event <- length(event_rows)
  
  event_ret_cum_mat <- ret_cum_mat[event_rows, , drop = FALSE]
  # event_ret_step_mat <- ret_step_mat[event_rows, , drop = FALSE]
  max_horizon_ret <- event_ret_cum_mat[, length(H)]
  
  pos_hit_mat <- event_ret_cum_mat >= threshold
  neg_hit_mat <- event_ret_cum_mat <= -threshold
  get_first_hit_idx <- function(row) which(row)[1]
  pos_hit_success <- apply(pos_hit_mat, 1, get_first_hit_idx) - apply(neg_hit_mat, 1, get_first_hit_idx) < 0
  
  # ---- summarized statistics ----
  
  qs <- quantile(max_horizon_ret, c(.1, .25, .5, .75, .9), na.rm = TRUE)
  res <- data.table::data.table(
    inst_id = attributes(DT)$inst_id,
    start_dt = start_dt,
    end_dt = end_dt,
    event = event_col,
    mean_horizon_ret = mean(max_horizon_ret, na.rm = TRUE),
    sd_horizon_ret = sd(max_horizon_ret, na.rm = TRUE),
    q10_horizon_ret = qs[1],
    q25_horizon_ret = qs[2],
    median_horizon_ret = qs[3],
    q75_horizon_ret = qs[4],
    q90_horizon_ret = qs[5],
    pos_hit_success_rate = length(which(pos_hit_success == TRUE)) / n_event,
    neg_hit_success_rate = length(which(pos_hit_success == FALSE)) / n_event
  )
  
  # ---- statistics by cases ----
  
  performance_by_cases <- data.table::data.table(
    pos_hit_success = pos_hit_success,
    max_horizon_ret = max_horizon_ret
  )
  if (length(event_score_cols) > 0) {
    for (event_score_col in event_score_cols) {
      performance_by_cases[[event_score_col]] <- DT[event_rows, event_score_col]
    }
  }
  
  res[['performance_by_cases']] <- list(performance_by_cases)
  
  # ---- statistics by horizon ----
  
  performance_by_horizons <- data.table::data.table(
    t(rbind(
      Horizon = H,
      Mean = apply(event_ret_cum_mat, 2, function(x) mean(x, na.rm = TRUE)),
      SD = apply(event_ret_cum_mat, 2, function(x) sd(x, na.rm = TRUE)),
      apply(event_ret_cum_mat, 2, function(x) quantile(x, c(.1, .25, .5, .75, .9), na.rm = TRUE))
    ))
  )
  
  res[['performance_by_horizons']] <- list(performance_by_horizons)

  return(invisible(res))
}

#' Plot event cumulative-return bands
#'
#' @param res result from \code{eval_event_performance()}.
#' @param threshold numeric; dashed reference lines at \eqn{\pm}threshold.
#' @return ggplot object.
#' @export
eval_event_plot_tsline_cum_ret <- function(res, threshold = 0.02) {
  wide_DT <- res$performance_by_horizons[[1]][, c('Horizon', '10%', '25%', '50%', '75%', '90%')]
  long_DT <- data.table::melt(wide_DT, id.vars = "Horizon", variable.name = "stat", value.name = "value")
  ggplot2::ggplot(long_DT, ggplot2::aes(x = Horizon, y = value, color = stat)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", color = "gray40") +
    ggplot2::geom_hline(yintercept = -threshold, linetype = "dashed", color = "gray40") +
    ggplot2::labs(
      title = "",
      x = "",
      y = "",
      color = NULL
    ) +
    ggplot2::theme_minimal()
}
