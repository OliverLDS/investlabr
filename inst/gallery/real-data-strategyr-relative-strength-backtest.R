library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

gallery_dir <- if (!is.null(sys.frames()[[1]]$ofile)) dirname(normalizePath(sys.frames()[[1]]$ofile)) else file.path(getwd(), "inst", "gallery")
source(file.path(gallery_dir, "strategyr-backtest-gallery-utils.R"))

ticker <- "IEFA"
benchmark_ticker <- "IVV"
asset_label <- "iShares Core MSCI EAFE ETF"
from_date <- as.Date("2023-01-01")
to_date <- Sys.Date()
target_max_leverage <- 0.95
account_leverage <- 1.0
contract_size <- 0.01
contract_step <- 0.01
fee_ratio <- 0.0007
funding_ratio <- 0
backtest_caption <- sprintf(
  "Backtest assumptions: traded asset = %s; relative-strength benchmark = %s; contract size = %.2f; contract step = %.2f; fee ratio = %.4f; funding ratio = %.4f; account leverage = %.1f; target max leverage = %.2f.",
  ticker, benchmark_ticker, contract_size, contract_step, fee_ratio, funding_ratio, account_leverage, target_max_leverage
)

# Optional sync if your local Yahoo cache is not up to date.
# investdatar::sync_local_quantmod_OHLC(ticker = ticker, from = as.character(from_date), to = as.character(to_date), src = "yahoo")
# investdatar::sync_local_quantmod_OHLC(ticker = benchmark_ticker, from = as.character(from_date), to = as.character(to_date), src = "yahoo")

market_dt <- load_strategyr_gallery_yahoo(ticker, from_date, to_date)
benchmark_dt <- load_strategyr_gallery_yahoo(benchmark_ticker, from_date, to_date)
benchmark_close <- benchmark_dt[, .(date = as.Date(datetime), benchmark_close = close)]
market_dt[, date := as.Date(datetime)]
market_dt <- merge(market_dt, benchmark_close, by = "date", all = FALSE)[order(datetime)]
market_dt[, date := NULL]
if (nrow(market_dt) < 120L) stop("Need at least 120 merged rows for the relative-strength gallery example.")

relative_strength_tgt <- strategyr::strat_relative_strength_tgt_pos(
  market_dt,
  x_col = "close",
  y_col = "benchmark_close",
  n = 20L,
  long_threshold = 1.02,
  short_threshold = 0.98,
  target_size = target_max_leverage,
  compute_features = TRUE
)
buy_hold_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = target_max_leverage)

bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(market_dt, relative_strength_tgt, 503L, account_leverage, contract_size, contract_step, fee_ratio, funding_ratio),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "Relative strength vs IVV | n = 20, thresholds = 1.02/0.98, target max lev = %.2f, account lev = %.1f",
    target_max_leverage, account_leverage
  )
)
benchmark_bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(market_dt, buy_hold_tgt, 1L, account_leverage, contract_size, contract_step, fee_ratio, funding_ratio, tol_pos = 0),
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
  "This example evaluates a relative-strength strategy using IEFA as the traded asset and IVV as the benchmark. The rule goes long when IEFA has sufficiently strong rolling relative performance and short when it is sufficiently weak, then compares the path-dependent backtest against buy-and-hold IEFA exposure.\n\n"
)

print(plot)
