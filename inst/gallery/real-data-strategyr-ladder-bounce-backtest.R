library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

gallery_dir <- if (!is.null(sys.frames()[[1]]$ofile)) dirname(normalizePath(sys.frames()[[1]]$ofile)) else file.path(getwd(), "inst", "gallery")
source(file.path(gallery_dir, "strategyr-backtest-gallery-utils.R"))

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
  contract_size, contract_step, fee_ratio, funding_ratio, account_leverage, target_max_leverage
)

# Optional sync if your local Yahoo cache is not up to date.
# investdatar::sync_local_quantmod_OHLC(ticker = ticker, from = as.character(from_date), to = as.character(to_date), src = "yahoo")

market_dt <- load_strategyr_gallery_yahoo(ticker, from_date, to_date)
ladder_tgt <- strategyr::strat_ladder_bounce_tgt_pos(
  market_dt,
  cycle_N = 180L,
  span = 3L,
  refined = TRUE,
  min_swing = 0.05,
  center_idx = 9L,
  lower = 7L,
  upper = 13L,
  target_size = target_max_leverage,
  compute_ladder = TRUE
)
benchmark_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = target_max_leverage)

bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(market_dt, ladder_tgt, 201L, account_leverage, contract_size, contract_step, fee_ratio, funding_ratio),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "Ladder bounce | cycle = 180, lower/upper = 7/13, target max lev = %.2f, account lev = %.1f",
    target_max_leverage, account_leverage
  )
)
benchmark_bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(market_dt, benchmark_tgt, 1L, account_leverage, contract_size, contract_step, fee_ratio, funding_ratio, tol_pos = 0),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf("Buy and hold | target max lev = %.2f, account lev = %.1f", target_max_leverage, account_leverage)
)

plot <- investlabr::eval_strat_plot_tsline_eq(
  bt_res,
  benchmark_bt_res = benchmark_bt_res,
  style = "macro_classic",
  context = "report",
  caption = backtest_caption
)

cat(
  "This example evaluates a ladder-bounce strategy on the S&P 500. The rule interprets extreme Fibonacci ladder zones as mean-reversion opportunities and compares the path-dependent strategyr backtest against buy-and-hold.\n\n"
)

print(plot)
