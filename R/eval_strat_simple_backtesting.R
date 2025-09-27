#' Position → per-bar log returns
#'
#' @param pos numeric vector of positions (e.g., -1..1).
#' @param close numeric close prices.
#' @param open numeric open prices.
#' @param fee_rate numeric fee per unit absolute position change.
#' @param mode one of \code{"new_open"} or \code{"last_close"}; pricing basis.
#' @return Numeric vector of log returns (same length as inputs).
#' @keywords internal
#' @noRd
.pos_to_log_ret <- function(pos, close, open, fee_rate, mode = c('new_open', 'last_close')) {
  mode <- match.arg(mode)
  
  lag_pos   <- data.table::shift(pos, type = "lag", fill = 0L)
  lag_close <- data.table::shift(close, type = "lag")
  
  pos_change <- abs(pos - lag_pos)
  fee_cost <- fee_rate * pos_change
  
  if (mode == 'new_open') {
    log_ret <- lag_pos * log((close - fee_cost) / open)  
  } else {
    log_ret <- lag_pos * log((close - fee_cost) / lag_close)
  }

  log_ret[is.na(log_ret)] <- 0
  return(log_ret)
}

#' Log-returns → equity curve
#'
#' @param log_ret numeric vector of log returns.
#' @param initial_equity numeric starting equity.
#' @return Numeric vector of equity values.
#' @keywords internal
#' @noRd
.log_ret_to_equity <- function(log_ret, initial_equity = 1) {
  exp(cumsum(log_ret)) * initial_equity
}

#' Build backtest result row from log returns
#'
#' @param datetime POSIXct vector (strictly increasing).
#' @param log_ret numeric log returns aligned with \code{datetime}.
#' @param asset_name string.
#' @param bg_time,ed_time POSIXct; inclusive start, exclusive end (may be \code{NA}).
#' @param strat_name string; strategy name.
#' @param strat_par list; parameters.
#' @param strat_label string; label (e.g., position column).
#' @param rf_rate numeric annual risk-free rate.
#' @return data.table (single row) with summary metrics and list-col \code{log_ret_dt}.
#' @keywords internal
#' @noRd
.log_ret_to_bt_res <- function(datetime, log_ret, asset_name, bg_time, ed_time, strat_name, strat_par, strat_label, rf_rate) {
  
  stopifnot(length(datetime) == length(log_ret))
  d <- diff(datetime)
  stopifnot(all(d > 0))
  d_days <- as.numeric(d, units = "days")
  avg_gap_days <- unique(d_days) # here we calculate time gap between bars
  stopifnot(length(avg_gap_days) == 1L)
  
  sel <- rep(TRUE, length(datetime))
  if (!is.na(bg_time)) sel <- sel & datetime >= bg_time
  if (!is.na(ed_time)) sel <- sel & datetime < ed_time # the ed_time will not be included in duration
  
  datetime <- datetime[sel]
  log_ret  <- log_ret[sel]
  
  start_dt <- datetime[1L] # could be different from bg_time
  end_dt <- datetime[length(datetime)] # definitely different from ed_time (included vs. excluded)
  total_days <- as.numeric(difftime(end_dt, start_dt, units = "days")) + avg_gap_days
  total_years <- total_days / 365.25
  
  raw_ret <- exp(sum(log_ret)) - 1
  ann_ret <- exp(sum(log_ret)/total_years) - 1
  
  ann_ret_sd <- stats::sd(log_ret) * sqrt(365.25 / avg_gap_days)
  sharpe <- ifelse(ann_ret_sd == 0, NA_real_, (ann_ret - rf_rate) / ann_ret_sd)
  
  ann_ret_sd_downside <- stats::sd(pmin(log_ret, 0)) * sqrt(365.25 / avg_gap_days)
  sortino <- ifelse(ann_ret_sd_downside == 0, NA_real_, (ann_ret - rf_rate) / ann_ret_sd_downside)
  
  equity <- .log_ret_to_equity(log_ret)
  dd <- 1 - equity / cummax(equity)
  max_dd <- max(dd, na.rm = TRUE)
  max_dd_duration <- max(rle(dd > 0)$lengths) * avg_gap_days # days
  
  data.table::data.table(
    asset_name = asset_name,
    bg_time = bg_time,
    ed_time = ed_time,
    
    strat_name = strat_name,
    strat_par = strat_par,
    strat_label = strat_label,
    
    start = start_dt,
    end = end_dt,
    total_years = total_years,
    total_return = raw_ret,
    annual_return = ann_ret,
    max_drawdown = max_dd,
    max_dd_duration = max_dd_duration,
    annual_volatility = ann_ret_sd,
    sharpe_ratio = sharpe,
    sortino_ratio = sortino,
    
    log_ret_dt = list(data.table::data.table(datetime = datetime, log_ret = log_ret))
  )
}

#' Evaluate strategy performance
#'
#' @param DT data.table with \code{datetime}, \code{open}, \code{close}, and a position column.
#' @param pos_col_name string; name of the position column in \code{DT}.
#' @param bg_time,ed_time POSIXct; inclusive start, exclusive end; may be \code{NA}.
#' @param fee_rate numeric fee per unit abs(position change).
#' @param rf_rate numeric annual risk-free rate.
#' @param mode one of \code{"new_open"}, \code{"last_close"}; execution pricing basis.
#' @return Invisibly: data.table (one row) with metrics and list-col \code{log_ret_dt}.
#' @export
eval_strat_performance <- function(DT, pos_col_name, bg_time = as.POSIXct(NA), ed_time = as.POSIXct(NA), fee_rate = 0, rf_rate = 0, mode = c("new_open", "last_close")) {
  
  mode <- match.arg(mode)
  stopifnot(all(pos_col_name %in% names(DT)))
  
  # here we need a function from pos_col_name to strat_name, strat_par, and strat_label
  
  datetime <- DT[['datetime']]
  close <- DT[["close"]]
  open <- DT[["open"]]
  pos <- DT[[pos_col_name]] #----- add pos and event to DT since it is easy to debug ----
  
  log_ret <- .pos_to_log_ret(pos, close, open, fee_rate = fee_rate, mode = mode) # assume the position order based on previous bar and executed at the price of new open
  
  # n_pos_changes <- sum(diff(pos) != 0)
  
  inst_id <- attributes(DT)$inst_id
  if (! is.na(bg_time)) bg_time <- as.POSIXct(bg_time)
  if (! is.na(ed_time)) ed_time <- as.POSIXct(ed_time)
  strat_name <- attributes(pos)$strat_name
  strat_par <- attributes(pos)$strat_par

  bt_res <- .log_ret_to_bt_res(datetime, log_ret, 
    asset_name = inst_id, bg_time = bg_time, ed_time = ed_time,
    strat_name = strat_name, strat_par = list(strat_par), strat_label = pos_col_name, 
    rf_rate = rf_rate)
  
  return(invisible(bt_res))
}

#' Scatter: annual return vs max drawdown
#'
#' @param bt_res_list data.table of backtest rows (from \code{eval_strat_performance()}).
#' @param plot_title string.
#' @param opt_portfolio_line logical; unused placeholder.
#' @return ggplot object.
#' @export
eval_strat_plot_scatter_maxdd_annret <- function(bt_res_list, plot_title = '', opt_portfolio_line = FALSE) {
  bt_res_list |> ggplot2::ggplot(ggplot2::aes(x = max_drawdown, y = annual_return, color = asset_name)) +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(ggplot2::aes(label = sprintf("%s_%s", substr(asset_name, 1, 3), strat_label)), vjust = -0.5, size = 3) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(x = 'MoD', y = 'Ret', title = plot_title) +
    ggplot2::theme_minimal()
}

#' Plot equity curve (time series)
#'
#' @param bt_res single-row data.table from \code{eval_strat_performance()}.
#' @return ggplot object.
#' @export
eval_strat_plot_tsline_eq <- function(bt_res) {
  bt_dt <- bt_res$log_ret_dt[[1]]
  data.table::set(bt_dt, j = 'eq', value = .log_ret_to_equity(bt_dt$log_ret))
  bt_dt |> ggplot2::ggplot(ggplot2::aes(x = datetime, y = eq)) +
    ggplot2::geom_line() +
    ggplot2::labs(x='', y='', 
      title = sprintf("%s | %s", bt_res$asset_name, bt_res$strat_label),
      subtitle = sprintf("%s ~ %s", format(bt_res$start, '%Y-%m-%d'), format(bt_res$end, '%Y-%m-%d')),
      caption = sprintf("total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(bt_res$total_return*100, 2), round(bt_res$annual_return*100, 2), round(bt_res$max_drawdown*100, 2))) +
    ggplot2::theme_minimal()
}

