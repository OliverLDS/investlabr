.pos_to_transactions <- function(datetime, open, close, pos, mode = c('new_open', 'last_close'), debug_mode = FALSE, leverage = 1, fee_rate = 0.0007, fee_sides = 1L) {
  
  mode <- match.arg(mode)
  
  lag_pos <- data.table::shift(pos, type = "lag", fill = 0)

  same_side_pos <- ifelse(sign(pos) == sign(lag_pos), pos, 0)
  same_side_lag_pos <- ifelse(sign(pos) == sign(lag_pos), lag_pos, 0)

  # Quantities to close/open (absolute, >= 0)
  close_qty <- pmax(0, abs(lag_pos) - abs(same_side_pos))
  open_qty  <- pmax(0, abs(pos) - abs(same_side_lag_pos))

  # Indices with actual trades
  idx_close <- which(close_qty > 0)
  idx_open  <- which(open_qty  > 0)
  
  if (mode == 'new_open') price_col <- open else price_col <- close

  # Build close legs
  if (length(idx_close)) {
    tx_close <- data.table::data.table(
      datetime  = as.character(datetime[idx_close]),
      action    = "close",
      direction = ifelse(lag_pos[idx_close] > 0, "long", "short"),
      size      = close_qty[idx_close],
      price     = price_col[idx_close]
    )
  } else {
    tx_close <- data.table::data.table(
      datetime = character(),
      action = character(),
      direction = character(),
      size = numeric(),
      price = numeric()
    )
  }

  # Build open legs
  if (length(idx_open)) {
    tx_open <- data.table::data.table(
      datetime  = as.character(datetime[idx_open]),
      action    = "open",
      direction = ifelse(pos[idx_open] > 0, "long", "short"),
      size      = open_qty[idx_open],
      price     = price_col[idx_open]
    )
  } else {
    tx_open <- data.table::data.table(
      datetime = character(),
      action = character(),
      direction = character(),
      size = numeric(),
      price = numeric()
    )
  }

  # Concatenate and order by time (opens and closes at same bar both execute at bar open)
  out <- data.table::rbindlist(list(tx_close, tx_open), use.names = TRUE)
  if (nrow(out)<=0) return(NULL)
  
  data.table::setorder(out, datetime)
  
  if (debug_mode) {
    output_stats <- c("balance_long_size", "balance_short_size", "average_long_price", "average_short_price", "pre_fee_log_ret", "cumulative_pre_fee_pnl_ratio", "is_win", "cumulative_win_rate")
    } else {
      output_stats <- c("pre_fee_log_ret", "cumulative_pre_fee_pnl_ratio", "is_win", "cumulative_win_rate")
  }
  
  out[, (output_stats) :=
    {
      n  <- .N
      bl <- rep(NA_real_, n)  # balance_long_size
      bs <- rep(NA_real_, n)  # balance_short_size
      al <- rep(NA_real_, n)  # average_long_price
      as <- rep(NA_real_, n)  # average_short_price
      lr <- rep(NA_real_, n)  # pre_fee_log_ret
      cr <- rep(NA_real_, n)  # cumulative_pnl_ratio
      iw <- rep(NA_real_, n)  # is_win
      wr <- rep(NA_real_, n)  # cumulative_win_rate
  
      long_size  <- 0 # when your leverage > 1, the size is not the real size you placed but just the portion your mortgage used  
      long_cost  <- 0  # sum(size * entry_price) for current long position
      short_size <- 0
      short_cost <- 0  # sum(size * entry_price) for current short position
  
      cum_logr    <- 0
      cum_pnl_ratio <- 0
      n_closed   <- 0L
      n_wins     <- 0L
  
      for (i in seq_len(n)) {
        act <- action[i]
        dir <- direction[i]
        sz  <- size[i]
        px  <- price[i]
  
        if (dir == "long") {
          if (act == "open") {
            long_size <- long_size + sz
            long_cost <- long_cost + sz * px
            logr <- NA_real_
            is_win <- NA_real_
          } else { # close long
            if (long_size <= 0) stop("Closing long with no existing long position")
            
            avg_entry  <- long_cost / long_size
            fee_frac <- fee_rate * fee_sides * sz * avg_entry * leverage
            pnl_ratio <- ((px - avg_entry) * sz * leverage - fee_frac) / avg_entry
            logr  <- log1p(pnl_ratio)                    # log return
            cum_logr <- cum_logr + logr              # cumulative log return
            cum_pnl_ratio <- exp(cum_logr) - 1           # geometric compounded PnL ratio

            n_closed <- n_closed + 1L
            if (pnl_ratio > 0) {
              is_win <- 1
              n_wins <- n_wins + 1L
            } else {
              is_win <- 0
            }
            
            long_size <- long_size - sz
            long_cost <- long_cost - avg_entry * sz
          }
  
        } else if (dir == "short") {
          if (act == "open") {
            short_size <- short_size + sz
            short_cost <- short_cost + sz * px
            logr <- NA_real_
            is_win <- NA_real_
          } else { # close short
            if (short_size <= 0) stop("Closing short with no existing short position")
            
            avg_entry  <- short_cost / short_size
            fee_frac <- fee_rate * fee_sides * sz * avg_entry * leverage
            pnl_ratio <- ((avg_entry - px) * sz * leverage - fee_frac) / px
            logr  <- log1p(pnl_ratio)                    # log return
            cum_logr <- cum_logr + logr              # cumulative log return
            cum_pnl_ratio <- exp(cum_logr) - 1           # geometric compounded PnL ratio
  
            n_closed <- n_closed + 1L
            if (pnl_ratio > 0) {
              is_win <- 1
              n_wins <- n_wins + 1L
            } else {
              is_win <- 0
            }
  
            short_size <- short_size - sz
            short_cost <- short_cost - avg_entry * sz
          }
        }
  
        bl[i] <- long_size
        bs[i] <- short_size
  
        al[i] <- if (long_size  > 0) long_cost  / long_size  else NA_real_
        as[i] <- if (short_size > 0) short_cost / short_size else NA_real_
  
        lr[i] <- logr
        cr[i] <- cum_pnl_ratio
        iw[i] <- is_win
        wr[i] <- if (n_closed > 0) n_wins / n_closed else NA_real_
      }
  
      if (debug_mode) return(list(bl, bs, al, as, lr, cr, iw, wr)) else return(list(lr, cr, iw, wr))
    }
  ]
  
  out[]
}

#' @export
gen_rolling_monthly_stats <- function(transaction_dt, N_rolling = 6L, debug_mode = FALSE) {
  trades <- transaction_dt[action == "close", .(datetime, pre_fee_log_ret, is_win)]
  trades[, month := as.Date(format(as.POSIXct(datetime), "%Y-%m-01"))]
  monthly <- trades[
    ,
    .(
      sum_log_ret = sum(pre_fee_log_ret, na.rm = TRUE),   # sum of log returns in the month
      wins        = sum(is_win),
      trades      = .N                            # number of closed trades in the month
    ),
    by = month
  ][order(month)]
  full_months <- data.table(
    month = seq(min(monthly$month), max(monthly$month), by = "month")
  )
  monthly <- monthly[full_months, on = "month"]
  monthly[, sum_log_ret_ms := frollsum(sum_log_ret, N_rolling, align = "right", na.rm = TRUE)]
  monthly[, wins_ms        := frollsum(wins,        N_rolling, align = "right", na.rm = TRUE)]
  monthly[, trades_ms      := frollsum(trades,      N_rolling, align = "right", na.rm = TRUE)]
  monthly[, months_in_window := pmin(N_rolling, seq_len(.N))]
  monthly[, geom_ret_ms :=
    fifelse(trades_ms > 0, exp(sum_log_ret_ms) - 1, NA_real_)
  ]
  monthly[, win_rate_ms :=
    fifelse(trades_ms > 0, wins_ms / trades_ms, NA_real_)
  ]
  monthly[, avg_trades_ms :=
    fifelse(trades_ms > 0, trades_ms / months_in_window, NA_real_)
  ]
  monthly_ms_stats <- monthly[
    ,
    .(
      month,
      geom_ret_ms,      # geometric compounded return over last 12 months
      win_rate_ms,      # win rate over last 12 months
      avg_trades_ms     # avg number of closed trades per month over last 12 months
    )
  ]
  if (debug_mode) return(monthly) else return(monthly_ms_stats)
}


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
.pos_to_log_ret <- function(pos, close, open, fee_rate = 0.0007, leverage = 1, funding_rate = 0.00004, interest_rate = 0.00005, mode = c('new_open', 'last_close'), fee_sides = 1L) {
  mode <- match.arg(mode)
  
  lag_pos   <- data.table::shift(pos, type = "lag", fill = 0L)
  lag_open <- data.table::shift(open, type = "lag")
  lag_close <- data.table::shift(close, type = "lag")
  
  r_simple <- if (mode == "new_open") (open / lag_open) - 1 else (close / lag_close) - 1
  
  turnover <- abs(pos - lag_pos)
  fee_frac <- fee_rate * fee_sides * turnover * leverage
  
  carry_cost <- abs(lag_pos) * (funding_rate + interest_rate) * leverage
  
  log_ret <- log(lag_pos * leverage * r_simple - fee_frac - carry_cost + 1)
  
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
.log_ret_to_bt_res <- function(log_ret, datetime, asset_name = 'Asset Name', bg_time = as.POSIXct(NA), ed_time = as.POSIXct(NA), strat_name = 'Strategy Name', strat_par = 'Strategy Parameters', strat_label = 'Strategy Label', rf_rate = 0) {
  
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
eval_strat_performance <- function(DT, pos_col_name, bg_time = as.POSIXct(NA), ed_time = as.POSIXct(NA), fee_rate = 0.0007, funding_rate = 0.00004, interest_rate = 0.00005, leverage = 1, rf_rate = 0, mode = c("new_open", "last_close"), fee_sides = 1L, tz = Sys.timezone()) {
  
  mode <- match.arg(mode)
  stopifnot(all(pos_col_name %in% names(DT)))
  
  # here we need a function from pos_col_name to strat_name, strat_par, and strat_label
  
  datetime <- DT[['datetime']]
  close <- DT[["close"]]
  open <- DT[["open"]]
  pos <- DT[[pos_col_name]] #----- add pos and event to DT since it is easy to debug ----
  
  log_ret <- .pos_to_log_ret(pos, close, open, fee_rate = fee_rate, funding_rate = funding_rate, interest_rate = interest_rate, leverage = leverage, mode = mode, fee_sides = fee_sides) # assume the position order based on previous bar and executed at the price of new open
  
  # n_pos_changes <- sum(diff(pos) != 0)
  
  inst_id <- attributes(DT)$inst_id
  if (! is.na(bg_time)) bg_time <- as.POSIXct(bg_time, tz = tz)
  if (! is.na(ed_time)) ed_time <- as.POSIXct(ed_time, tz = tz)
  strat_name <- attributes(pos)$strat_name
  strat_par <- attributes(pos)$strat_par

  bt_res <- .log_ret_to_bt_res(log_ret, datetime, 
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
eval_strat_plot_tsline_eq <- function(bt_res, benchmark_bt_res = NULL) {
  bt_dt <- bt_res$log_ret_dt[[1]]
  data.table::set(bt_dt, j = 'eq', value = .log_ret_to_equity(bt_dt$log_ret))
  p <- bt_dt |> ggplot2::ggplot(ggplot2::aes(x = datetime, y = eq)) + ggplot2::geom_line(color = '#E69F00')
  title <- sprintf("%s | %s", bt_res$asset_name, bt_res$strat_label)
  subtitle <- sprintf("%s ~ %s", format(bt_res$start, '%Y-%m-%d'), format(bt_res$end, '%Y-%m-%d'))
  caption <- sprintf("Evaluated | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(bt_res$total_return*100, 2), round(bt_res$annual_return*100, 2), round(bt_res$max_drawdown*100, 2))
  if (!is.null(benchmark_bt_res)) {
    benchmark_bt_dt <- benchmark_bt_res$log_ret_dt[[1]]
    data.table::set(benchmark_bt_dt, j = 'eq', value = .log_ret_to_equity(benchmark_bt_dt$log_ret))
    p <- p + ggplot2::geom_line(data = benchmark_bt_dt, ggplot2::aes(x = datetime, y = eq), color = '#7F7F7F')
    caption <- paste0(caption, sprintf("\nBenchmark | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(benchmark_bt_res$total_return*100, 2), round(benchmark_bt_res$annual_return*100, 2), round(benchmark_bt_res$max_drawdown*100, 2)))
  }
  p <- p + ggplot2::labs(x='', y='', 
      title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_minimal()
  return(p)
}

