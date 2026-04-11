library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

ticker <- "^GSPC"
asset_label <- "S&P 500"
from_date <- as.Date("2023-01-01")
to_date <- Sys.Date()
target_max_leverage <- 0.95
account_leverage <- 1.0
contract_size <- 0.01
contract_step <- 0.01
fee_ratio <- 0.0007
funding_ratio <- 0
backtest_caption <- sprintf(
  "Backtest assumptions: contract size = %.2f; contract step = %.2f; fee ratio = %.4f; funding ratio = %.4f; account leverage = %.1f; target max leverage = %.2f.",
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

market_dt <- data.table::as.data.table(investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo"))
market_dt <- market_dt[datetime >= as.POSIXct(from_date) & datetime < as.POSIXct(to_date + 1)][order(datetime)]
if (nrow(market_dt) < 120L) stop("Need at least 120 rows of local OHLC data for this gallery example.")

macd_tgt <- strategyr::strat_macd_cross_tgt_pos(
  market_dt,
  fast = 12L,
  slow = 26L,
  signal = 9L,
  target_size = target_max_leverage,
  compute_features = TRUE
)
macd_contrarian_tgt <- -macd_tgt
benchmark_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = target_max_leverage)

bt_res <- build_plot_bt_res(
  equity = run_strategyr_backtest(market_dt, macd_tgt, strat_id = 304L),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "MACD cross | fast = 12, slow = 26, signal = 9, target max lev = %.2f, account lev = %.1f",
    target_max_leverage,
    account_leverage
  )
)
bt_res_2 <- build_plot_bt_res(
  equity = run_strategyr_backtest(market_dt, macd_contrarian_tgt, strat_id = 305L),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "MACD contrarian | fast = 12, slow = 26, signal = 9, target max lev = %.2f, account lev = %.1f",
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
  bt_res_2 = bt_res_2,
  style = "macro_classic",
  context = "report",
  caption = backtest_caption
)

cat(
  "This example evaluates a MACD-cross strategy and its contrarian inverse on the S&P 500. The chart compares standard trend-following MACD exposure, the opposite target-position path, and buy-and-hold to show whether poor directional timing is strategy-specific or simply a hard market regime for the rule.\n\n"
)

print(plot)
