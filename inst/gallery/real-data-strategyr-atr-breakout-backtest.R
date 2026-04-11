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
atr_tgt <- strategyr::strat_atr_breakout_tgt_pos(
  market_dt,
  n = 14L,
  atr_mult = 1,
  target_size = target_max_leverage,
  compute_features = TRUE
)
benchmark_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = target_max_leverage)

bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(market_dt, atr_tgt, 401L, account_leverage, contract_size, contract_step, fee_ratio, funding_ratio),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf("ATR breakout | n = 14, ATR mult = 1, target max lev = %.2f, account lev = %.1f", target_max_leverage, account_leverage)
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
  "This example evaluates an ATR breakout strategy on the S&P 500. The rule targets a long position after an upside move larger than one prior ATR and a short position after a downside move larger than one prior ATR, then compares the path-dependent strategyr backtest against a buy-and-hold S&P 500 benchmark.\n\n"
)

print(plot)
