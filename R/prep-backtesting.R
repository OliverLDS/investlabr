#' Build A Backtest Result Row From An Equity Curve
#'
#' Converts an externally generated equity curve into the compact backtest
#' result shape used by investlabr plotting helpers.
#'
#' @param equity Numeric equity curve.
#' @param datetime POSIXct-compatible vector aligned with `equity`.
#' @param asset_name Character asset label.
#' @param strat_label Character strategy label.
#'
#' @return A one-row `data.table` with summary metrics and a `log_ret_dt`
#'   list-column.
#' @export
prep_backtest_result_from_equity <- function(equity, datetime, asset_name, strat_label) {
  stopifnot(length(equity) == length(datetime), all(equity > 0))
  log_ret <- c(0, diff(log(equity)))
  eq_norm <- equity / equity[1]
  drawdown <- 1 - eq_norm / cummax(eq_norm)
  total_years <- max(as.numeric(difftime(max(datetime), min(datetime), units = "days")) / 365.25, 1 / 365.25)
  annual_return <- tail(eq_norm, 1)^(1 / total_years) - 1
  annualization <- max((length(log_ret) - 1) / total_years, 1)
  annual_volatility <- stats::sd(log_ret, na.rm = TRUE) * sqrt(annualization)
  downside_volatility <- stats::sd(pmin(log_ret, 0), na.rm = TRUE) * sqrt(annualization)

  data.table::data.table(
    asset_name = asset_name,
    strat_label = strat_label,
    start = min(datetime),
    end = max(datetime),
    total_return = tail(eq_norm, 1) - 1,
    annual_return = annual_return,
    max_drawdown = max(drawdown, na.rm = TRUE),
    annual_volatility = annual_volatility,
    sharpe_ratio = ifelse(annual_volatility == 0, NA_real_, annual_return / annual_volatility),
    sortino_ratio = ifelse(downside_volatility == 0, NA_real_, annual_return / downside_volatility),
    log_ret_dt = list(data.table::data.table(datetime = datetime, log_ret = log_ret))
  )
}

#' Build Synthetic OHLC For A Price Ratio
#'
#' Builds a close-aligned OHLC proxy for a ratio between two OHLC series. This
#' is intended for research-side spread or curve proxy visualization, not
#' production execution.
#'
#' @param numerator_dt,denominator_dt `data.table`-like OHLC inputs containing
#'   `datetime`, `open`, `high`, `low`, `close`, and optionally `volume`.
#' @param ratio_symbol Character label for the synthetic ratio.
#'
#' @return A `data.table` with OHLC columns for the synthetic ratio.
#' @export
prep_ratio_ohlc <- function(numerator_dt, denominator_dt, ratio_symbol) {
  numerator <- data.table::as.data.table(data.table::copy(numerator_dt))
  denominator <- data.table::as.data.table(data.table::copy(denominator_dt))
  required <- c("datetime", "open", "high", "low", "close")
  stopifnot(all(required %in% names(numerator)), all(required %in% names(denominator)))
  if (!"volume" %in% names(numerator)) numerator[, volume := NA_real_]
  if (!"volume" %in% names(denominator)) denominator[, volume := NA_real_]

  numerator[, date := as.Date(datetime)]
  denominator[, date := as.Date(datetime)]
  ratio_dt <- merge(
    numerator[, .(
      date,
      datetime,
      open_num = open,
      high_num = high,
      low_num = low,
      close_num = close,
      volume_num = volume
    )],
    denominator[, .(
      date,
      open_den = open,
      high_den = high,
      low_den = low,
      close_den = close,
      volume_den = volume
    )],
    by = "date",
    all = FALSE
  )[order(datetime)]
  ratio_dt[, `:=`(
    source = "synthetic_ratio",
    symbol = ratio_symbol,
    interval = "1d",
    open = open_num / open_den,
    high = pmax(open_num / open_den, close_num / close_den, high_num / low_den, na.rm = TRUE),
    low = pmin(open_num / open_den, close_num / close_den, low_num / high_den, na.rm = TRUE),
    close = close_num / close_den,
    volume = pmin(volume_num, volume_den, na.rm = TRUE),
    adj_close = close_num / close_den
  )]
  ratio_dt[, .(source, symbol, interval, datetime, date, open, high, low, close, volume, adj_close)]
}
