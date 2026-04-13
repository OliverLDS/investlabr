library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

ticker <- "CL=F"
asset_label <- "Crude Oil Futures"
from_date <- as.Date("2020-01-01")
to_date <- as.Date("2020-12-31")
target_max_leverage <- 0.95
account_leverage <- 1.0
contract_size <- 0.01
contract_step <- 0.01
fee_ratio <- 0.0007
funding_ratio <- 0
bollinger_n <- 15L
bollinger_k <- 3.0
warmup_start_date <- from_date - 365L * 2L
backtest_caption <- sprintf(
  "Backtest assumptions: traded asset = %s; Bollinger n = %s; k = %s; contract size = %.2f; contract step = %.2f; fee ratio = %.4f; funding ratio = %.4f; account leverage = %.1f; target max leverage = %.2f.",
  ticker,
  bollinger_n,
  bollinger_k,
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

build_plot_bt_res <- function(equity, datetime, asset_name, strat_label) {
  investlabr::prep_backtest_result_from_equity(equity, datetime, asset_name, strat_label)
}

run_strategyr_backtest <- function(market_dt, tgt_pos, strat_id) {
  strategyr::backtest_rcpp(
    timestamp = as.numeric(market_dt$datetime),
    open = market_dt$open,
    high = market_dt$high,
    low = market_dt$low,
    close = market_dt$close,
    tgt_pos = fifelse(is.na(tgt_pos), 0, tgt_pos),
    pos_strat = rep(as.integer(strat_id), nrow(market_dt)),
    tol_pos = rep(0.1, nrow(market_dt)),
    strat = as.integer(strat_id),
    asset = 8001L,
    ctr_size = contract_size,
    ctr_step = contract_step,
    lev = account_leverage,
    fee_rt = fee_ratio,
    fund_rt = funding_ratio,
    rec = FALSE
  )
}

market_full <- data.table::as.data.table(investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo"))
market_full <- market_full[datetime >= as.POSIXct(warmup_start_date) & datetime < as.POSIXct(to_date + 1)][order(datetime)]
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
  warning("No pre-period warmup rows are available; Bollinger signals may still cold-start at the trade window.")
}
market_dt <- market_full[trade_idx]
if (nrow(market_dt) < 120L) stop("Need at least 120 rows of local OHLC data for this gallery example.")
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

bollinger_tgt_full <- strategyr::strat_bollinger_revert_tgt_pos(
  market_full,
  n = bollinger_n,
  k = bollinger_k,
  target_size = target_max_leverage,
  compute_features = TRUE
)
bollinger_tgt <- bollinger_tgt_full[trade_idx]
benchmark_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = target_max_leverage)

bt_res <- build_plot_bt_res(
  equity = run_strategyr_backtest(market_dt, bollinger_tgt, strat_id = 301L),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "Bollinger reversion | n = %s, k = %s, target max lev = %.2f, account lev = %.1f",
    bollinger_n,
    bollinger_k,
    target_max_leverage,
    account_leverage
  )
)
benchmark_bt_res <- build_plot_bt_res(
  equity = run_strategyr_backtest(market_dt, benchmark_tgt, strat_id = 1L),
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
  "This example evaluates a Bollinger-band mean-reversion strategy on crude oil futures during 2020. The rule defines a 15-day moving-average band with upper and lower boundaries set three rolling standard deviations away from the moving average. When CL=F closes below the lower band, the strategy treats it as oversold and targets a long position; when it closes above the upper band, it treats it as overbought and targets a short position; as price normalizes, the strategy exits toward flat. Pre-2020 data is used only as signal warmup, while the displayed backtest is restricted to the 2020 trade window. The chart compares the path-dependent strategyr backtest against buy-and-hold CL=F over the same local Yahoo history window.\n\n"
)

print(plot)
