library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

ticker <- "^GSPC"
from_date <- as.Date("2023-01-01")
to_date <- Sys.Date()

# Optional sync if your local Yahoo cache is not up to date.
# investdatar::sync_local_quantmod_OHLC(
#   ticker = ticker,
#   from = as.character(from_date),
#   to = as.character(to_date),
#   src = "yahoo"
# )

build_plot_bt_res <- function(equity, datetime, asset_name, strat_label) {
  stopifnot(length(equity) == length(datetime), all(equity > 0))

  log_ret <- c(0, diff(log(equity)))
  eq_norm <- equity / equity[1]
  drawdown <- 1 - eq_norm / cummax(eq_norm)

  total_years <- as.numeric(difftime(max(datetime), min(datetime), units = "days")) / 365.25
  total_years <- max(total_years, 1 / 365.25)
  total_return <- tail(eq_norm, 1) - 1
  annual_return <- tail(eq_norm, 1)^(1 / total_years) - 1

  data.table(
    asset_name = asset_name,
    strat_label = strat_label,
    start = min(datetime),
    end = max(datetime),
    total_return = total_return,
    annual_return = annual_return,
    max_drawdown = max(drawdown, na.rm = TRUE),
    log_ret_dt = list(data.table(datetime = datetime, log_ret = log_ret))
  )
}

market_dt <- data.table::as.data.table(
  investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo")
)

if (is.null(market_dt) || nrow(market_dt) == 0L) {
  stop("Local Yahoo data not found for ticker: ", ticker)
}

market_dt <- market_dt[
  datetime >= as.POSIXct(from_date) &
    datetime < as.POSIXct(to_date + 1)
][order(datetime)]

if (nrow(market_dt) < 120L) {
  stop("Need at least 120 rows of local OHLC data for this gallery example.")
}

donchian_tgt <- strategyr::strat_donchian_breakout_tgt_pos(
  market_dt,
  n = 20L,
  target_size = 1.0,
  compute_features = TRUE
)

benchmark_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = 1.0)

donchian_eq <- strategyr::backtest_rcpp(
  timestamp = as.numeric(market_dt$datetime),
  open = market_dt$open,
  high = market_dt$high,
  low = market_dt$low,
  close = market_dt$close,
  tgt_pos = fifelse(is.na(donchian_tgt), 0, donchian_tgt),
  pos_strat = rep(302L, nrow(market_dt)),
  tol_pos = rep(0.1, nrow(market_dt)),
  strat = 302L,
  asset = 8001L,
  ctr_size = 1,
  ctr_step = 1,
  lev = 1,
  fee_rt = 0.0007,
  fund_rt = 0,
  rec = FALSE
)

benchmark_eq <- strategyr::backtest_rcpp(
  timestamp = as.numeric(market_dt$datetime),
  open = market_dt$open,
  high = market_dt$high,
  low = market_dt$low,
  close = market_dt$close,
  tgt_pos = fifelse(is.na(benchmark_tgt), 0, benchmark_tgt),
  pos_strat = rep(1L, nrow(market_dt)),
  tol_pos = rep(0, nrow(market_dt)),
  strat = 1L,
  asset = 8001L,
  ctr_size = 1,
  ctr_step = 1,
  lev = 1,
  fee_rt = 0.0007,
  fund_rt = 0,
  rec = FALSE
)

bt_res <- build_plot_bt_res(
  equity = donchian_eq,
  datetime = market_dt$datetime,
  asset_name = ticker,
  strat_label = "strategyr::strat_donchian_breakout_tgt_pos"
)

benchmark_bt_res <- build_plot_bt_res(
  equity = benchmark_eq,
  datetime = market_dt$datetime,
  asset_name = ticker,
  strat_label = "strategyr::strat_buy_and_hold_tgt_pos"
)

investlabr::eval_strat_plot_tsline_eq(
  bt_res,
  benchmark_bt_res = benchmark_bt_res,
  style = "macro_classic",
  context = "report"
)
