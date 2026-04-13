library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

gallery_dir <- if (!is.null(sys.frames()[[1]]$ofile)) dirname(normalizePath(sys.frames()[[1]]$ofile)) else file.path(getwd(), "inst", "gallery")
source(file.path(gallery_dir, "strategyr-backtest-gallery-utils.R"))

ticker <- "SOXX"
asset_label <- "iShares Semiconductor ETF"
from_date <- as.Date("2024-01-01")
to_date <- as.Date("2024-12-31")
target_max_leverage <- 0.95
account_leverage <- 1.0
contract_size <- 0.01
contract_step <- 0.01
fee_ratio <- 0.0007
funding_ratio <- 0
rsi_logr_h <- 18
rsi_logr_oversold <- 40
rsi_logr_overbought <- 65
rsi_logr_exit_level <- 47.5
warmup_start_date <- from_date - 365L * 2L
backtest_caption <- sprintf(
  "Backtest assumptions: traded asset = %s; log-return RSI h = %s; oversold = %s; overbought = %s; exit level = %s; contract size = %.2f; contract step = %.2f; fee ratio = %.4f; funding ratio = %.4f; account leverage = %.1f; target max leverage = %.2f.",
  ticker,
  rsi_logr_h,
  rsi_logr_oversold,
  rsi_logr_overbought,
  rsi_logr_exit_level,
  contract_size,
  contract_step,
  fee_ratio,
  funding_ratio,
  account_leverage,
  target_max_leverage
)

# Optional sync if your local Yahoo cache is not up to date.
# investdatar::sync_local_quantmod_OHLC(
#   ticker = ticker,
#   from = as.character(from_date),
#   to = as.character(to_date),
#   src = "yahoo"
# )

market_full <- load_strategyr_gallery_yahoo(ticker, warmup_start_date, to_date)

bad_ohlc <- !is.finite(market_full$open) |
  !is.finite(market_full$high) |
  !is.finite(market_full$low) |
  !is.finite(market_full$close) |
  market_full$open <= 0 |
  market_full$high <= 0 |
  market_full$low <= 0 |
  market_full$close <= 0
if (any(bad_ohlc)) {
  warning(
    sprintf(
      "Dropped %s %s rows with incomplete or non-positive OHLC values before signal construction/backtesting.",
      sum(bad_ohlc),
      ticker
    )
  )
  market_full <- market_full[!bad_ohlc]
}
trade_idx <- market_full$datetime >= as.POSIXct(from_date) & market_full$datetime < as.POSIXct(to_date + 1)
if (!any(market_full$datetime < as.POSIXct(from_date))) {
  warning("No pre-period warmup rows are available; log-return RSI signals may still cold-start at the trade window.")
}
market_dt <- market_full[trade_idx]
if (nrow(market_dt) < 120L) {
  stop("Need at least 120 valid rows of local OHLC data for this gallery example: ", ticker)
}
if (as.Date(min(market_dt$datetime)) > from_date) {
  warning(
    sprintf(
      "Local %s Yahoo cache starts on %s, after requested from_date %s. Sync older Yahoo data if you need the full intended backtest window.",
      ticker,
      as.character(as.Date(min(market_dt$datetime))),
      as.character(from_date)
    )
  )
}

rsi_logr_tgt_full <- strategyr::strat_rsi_logr_revert_tgt_pos(
  market_full,
  h = rsi_logr_h,
  oversold = rsi_logr_oversold,
  overbought = rsi_logr_overbought,
  exit_level = rsi_logr_exit_level,
  target_size = target_max_leverage,
  compute_features = TRUE
)
rsi_logr_tgt <- rsi_logr_tgt_full[trade_idx]
benchmark_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = target_max_leverage)

bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(
    market_dt,
    rsi_logr_tgt,
    306L,
    account_leverage,
    contract_size,
    contract_step,
    fee_ratio,
    funding_ratio
  ),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "Log-return RSI reversion | h = %s, %s/%s/%s, target max lev = %.2f, account lev = %.1f",
    rsi_logr_h,
    rsi_logr_oversold,
    rsi_logr_overbought,
    rsi_logr_exit_level,
    target_max_leverage,
    account_leverage
  )
)
benchmark_bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(
    market_dt,
    benchmark_tgt,
    1L,
    account_leverage,
    contract_size,
    contract_step,
    fee_ratio,
    funding_ratio,
    tol_pos = 0
  ),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "Buy and hold | target max lev = %.2f, account lev = %.1f",
    target_max_leverage,
    account_leverage
  )
)

plot <- investlabr::eval_strat_plot_tsline_eq(
  bt_res,
  benchmark_bt_res = benchmark_bt_res,
  style = "macro_classic",
  context = "report",
  caption = backtest_caption
)

cat(
  "This example evaluates strategyr's log-return RSI mean-reversion strategy on SOXX during 2024. Unlike a classic price-level RSI, the indicator is built from smoothed log-return momentum with half-life h = 18. The rule treats log-return RSI below 40 as oversold and targets a long SOXX position, treats log-return RSI above 65 as overbought and targets a short SOXX position, and flattens the open position once the indicator mean-reverts to the 47.5 exit level. Pre-2024 data is used only as signal warmup, while the displayed backtest is restricted to the 2024 trade window. The chart compares the path-dependent strategyr backtest against a buy-and-hold SOXX benchmark over the same local Yahoo history window.\n\n"
)

print(plot)
