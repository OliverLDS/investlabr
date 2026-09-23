#' @import data.table
#' @importFrom rlang .data
#' @importFrom stats cor cov quantile sd setNames
#' @importFrom utils tail
NULL

# Column symbols evaluated within data.table and ggplot2 data masks.
utils::globalVariables(c(
  ".", "action", "adj_close", "alpha", "annual_return", "asset_name",
  "atr_logr_12", "atr_q_10_12_300", "atr_q_20_12_300", "atr_q_80_12_300",
  "atr_q_90_12_300", "avg_trades_ms", "before_window", "candle_color",
  "close_den", "close_num", "color", "curve_date", "datetime", "direction",
  "ema_100", "ema_20", "ema_200", "ema_50", "geom_ret_ms", "high",
  "high_den", "high_num", "Horizon", "interval", "is_win", "kind", "label",
  "label_x", "log_maturity", "low", "low_den", "low_num", "maturity",
  "max_drawdown", "Mean", "metric", "months_in_window", "open_den",
  "open_num", "panel", "pos", "pre_fee_log_ret", "price", "series", "size",
  "stat", "strat_label", "sum_log_ret", "sum_log_ret_ms", "symbol",
  "trades_ms", "value", "volume", "volume_den", "volume_num", "win_rate_ms",
  "window_length", "wins", "wins_ms", "x", "x_lab_atr", "x_lab_ema", "xend",
  "y", "y_offset", "yield", "zone_center", "zone_high", "zone_low"
))
