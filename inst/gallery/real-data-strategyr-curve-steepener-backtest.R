library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

gallery_dir <- if (!is.null(sys.frames()[[1]]$ofile)) dirname(normalizePath(sys.frames()[[1]]$ofile)) else file.path(getwd(), "inst", "gallery")
source(file.path(gallery_dir, "strategyr-backtest-gallery-utils.R"))

short_ticker <- "SHY"
long_ticker <- "TLT"
asset_label <- "SHY/TLT Treasury ETF ratio"
from_date <- as.Date("2025-03-07")
to_date <- Sys.Date()
target_max_leverage <- 0.95
account_leverage <- 1.0
contract_size <- 0.01
contract_step <- 0.01
fee_ratio <- 0.0007
funding_ratio <- 0
backtest_caption <- sprintf(
  "Backtest assumptions: one trading-row-lagged Treasury 10Y-2Y slope drives the signal; tradable proxy = %s/%s ratio; contract size = %.2f; contract step = %.2f; fee ratio = %.4f; funding ratio = %.4f; account leverage = %.1f; target max leverage = %.2f.",
  short_ticker, long_ticker, contract_size, contract_step, fee_ratio, funding_ratio, account_leverage, target_max_leverage
)

# Optional sync if your local Treasury/Yahoo cache is not up to date.
# investdatar::sync_local_treasury_rates("par_yield_curve")
# investdatar::sync_local_quantmod_OHLC(ticker = short_ticker, from = as.character(from_date), to = as.character(to_date), src = "yahoo")
# investdatar::sync_local_quantmod_OHLC(ticker = long_ticker, from = as.character(from_date), to = as.character(to_date), src = "yahoo")

short_dt <- load_strategyr_gallery_yahoo(short_ticker, from_date, to_date)
long_dt <- load_strategyr_gallery_yahoo(long_ticker, from_date, to_date)
market_dt <- build_strategyr_gallery_ratio_ohlc(short_dt, long_dt, ratio_symbol = paste0(short_ticker, "/", long_ticker))

treasury_dt <- data.table::as.data.table(investdatar::get_local_treasury_rates("par_yield_curve"))
if (nrow(treasury_dt) == 0L) {
  stop("Local Treasury data not found for dataset: par_yield_curve")
}
curve_dt <- data.table::dcast(
  treasury_dt[
    measure == "yield" &
      tenor %in% c("2YEAR", "10YEAR") &
      as.Date(date) >= from_date &
      as.Date(date) <= to_date,
    .(date = as.Date(date), tenor, value = value / 100)
  ],
  date ~ tenor,
  value.var = "value"
)
data.table::setnames(curve_dt, c("2YEAR", "10YEAR"), c("short_rate", "long_rate"))
market_dt <- merge(market_dt, curve_dt, by = "date", all = FALSE)[order(datetime)]
market_dt[, `:=`(
  short_rate_signal = data.table::shift(short_rate, 1L),
  long_rate_signal = data.table::shift(long_rate, 1L)
)]
market_dt <- market_dt[!is.na(short_rate_signal) & !is.na(long_rate_signal)]
if (nrow(market_dt) < 120L) {
  stop("Need at least 120 merged rows for the curve-steepener gallery example.")
}

steepener_tgt <- strategyr::strat_curve_steepener_tgt_pos(
  market_dt,
  short_rate_col = "short_rate_signal",
  long_rate_col = "long_rate_signal",
  long_threshold = 0.006,
  short_threshold = 0.004,
  target_size = target_max_leverage,
  compute_features = TRUE
)
contrarian_steepener_tgt <- -steepener_tgt
benchmark_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = target_max_leverage)

bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(market_dt, steepener_tgt, 603L, account_leverage, contract_size, contract_step, fee_ratio, funding_ratio),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "Curve steepener | lagged 10Y-2Y, 60/40bp, tgt lev %.2f",
    target_max_leverage
  )
)
bt_res_2 <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(market_dt, contrarian_steepener_tgt, 613L, account_leverage, contract_size, contract_step, fee_ratio, funding_ratio),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf(
    "Contrarian steepener | inverse lagged 10Y-2Y, tgt lev %.2f",
    target_max_leverage
  )
)
benchmark_bt_res <- build_strategyr_gallery_bt_res(
  equity = run_strategyr_gallery_backtest(market_dt, benchmark_tgt, 1L, account_leverage, contract_size, contract_step, fee_ratio, funding_ratio, tol_pos = 0),
  datetime = market_dt$datetime,
  asset_name = asset_label,
  strat_label = sprintf("Buy and hold proxy | target max lev = %.2f, account lev = %.1f", target_max_leverage, account_leverage)
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
  "This example evaluates a Treasury curve-steepener signal and its contrarian inverse. The signal uses the prior merged trading row's U.S. Treasury 10Y-2Y slope with a 60bp/40bp neutral zone: if the lagged slope is above 60bp, the direct strategy takes a long SHY/TLT ratio position; if it is below 40bp, it takes a short SHY/TLT ratio position; inside the neutral zone it holds flat. The contrarian strategy flips those positions. The benchmark is buy-and-hold of the same synthetic SHY/TLT ratio proxy.\n\n"
)

print(plot)
