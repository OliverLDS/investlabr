library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

gallery_dir <- if (!is.null(sys.frames()[[1]]$ofile)) dirname(normalizePath(sys.frames()[[1]]$ofile)) else file.path(getwd(), "inst", "gallery")
source(file.path(gallery_dir, "strategyr-backtest-gallery-utils.R"))

target_max_leverage <- 0.95
account_leverage <- 1.0
contract_size <- 0.01
contract_step <- 0.01
fee_ratio <- 0.0007
funding_ratio <- 0

# Optional sync if your local Yahoo cache is not up to date.
# investdatar::sync_local_quantmod_OHLC(ticker = "XLU", from = "2003-01-01", to = "2003-12-31", src = "yahoo")
# investdatar::sync_local_quantmod_OHLC(ticker = "XLY", from = "2012-01-01", to = "2012-12-31", src = "yahoo")

drop_bad_ohlc <- function(market_dt, ticker) {
  bad_ohlc <- !is.finite(market_dt$open) |
    !is.finite(market_dt$high) |
    !is.finite(market_dt$low) |
    !is.finite(market_dt$close) |
    market_dt$open <= 0 |
    market_dt$high <= 0 |
    market_dt$low <= 0 |
    market_dt$close <= 0
  if (any(bad_ohlc)) {
    warning(
      sprintf(
        "Dropped %s %s rows with incomplete or non-positive OHLC values before backtesting.",
        sum(bad_ohlc),
        ticker
      )
    )
    market_dt <- market_dt[!bad_ohlc]
  }
  if (nrow(market_dt) < 120L) {
    stop("Need at least 120 valid rows of local OHLC data for this gallery example: ", ticker)
  }
  market_dt
}

make_macd_plot <- function(
  ticker,
  asset_label,
  from_date,
  to_date,
  strategy_label,
  strategy_fun,
  strat_id,
  fast,
  slow,
  signal
) {
  warmup_start_date <- from_date - 365L * 2L
  market_full <- load_strategyr_gallery_yahoo(ticker, warmup_start_date, to_date)
  market_full <- drop_bad_ohlc(market_full, ticker)
  trade_idx <- market_full$datetime >= as.POSIXct(from_date) & market_full$datetime < as.POSIXct(to_date + 1)
  if (!any(market_full$datetime < as.POSIXct(from_date))) {
    warning("No pre-period warmup rows are available; MACD signals may still cold-start at the trade window for ticker: ", ticker)
  }
  market_dt <- market_full[trade_idx]
  if (nrow(market_dt) < 120L) {
    stop("Need at least 120 trade-window rows of local OHLC data for this gallery example: ", ticker)
  }

  macd_tgt_full <- strategy_fun(
    market_full,
    fast = fast,
    slow = slow,
    signal = signal,
    target_size = target_max_leverage,
    compute_features = TRUE
  )
  macd_tgt <- macd_tgt_full[trade_idx]
  benchmark_tgt <- strategyr::strat_buy_and_hold_tgt_pos(market_dt, value = target_max_leverage)

  bt_res <- build_strategyr_gallery_bt_res(
    equity = run_strategyr_gallery_backtest(
      market_dt,
      macd_tgt,
      strat_id,
      account_leverage,
      contract_size,
      contract_step,
      fee_ratio,
      funding_ratio
    ),
    datetime = market_dt$datetime,
    asset_name = asset_label,
    strat_label = sprintf(
      "%s | %s/%s/%s, target max lev = %.2f, account lev = %.1f",
      strategy_label,
      fast,
      slow,
      signal,
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
    strat_label = sprintf("Buy and hold | target max lev = %.2f, account lev = %.1f", target_max_leverage, account_leverage)
  )

  backtest_caption <- sprintf(
    "Backtest assumptions: traded asset = %s; strategy = %s; MACD parameters = %s/%s/%s; period = %s to %s; contract size = %.2f; contract step = %.2f; fee ratio = %.4f; funding ratio = %.4f; account leverage = %.1f; target max leverage = %.2f.",
    ticker,
    strategy_label,
    fast,
    slow,
    signal,
    as.character(from_date),
    as.character(to_date),
    contract_size,
    contract_step,
    fee_ratio,
    funding_ratio,
    account_leverage,
    target_max_leverage
  )

  investlabr::eval_strat_plot_tsline_eq(
    bt_res,
    benchmark_bt_res = benchmark_bt_res,
    style = "macro_classic",
    context = "report",
    caption = backtest_caption
  )
}

macd_cross_plot <- make_macd_plot(
  ticker = "XLU",
  asset_label = "Utilities Select Sector SPDR Fund",
  from_date = as.Date("2003-01-01"),
  to_date = as.Date("2003-12-31"),
  strategy_label = "MACD cross",
  strategy_fun = strategyr::strat_macd_cross_tgt_pos,
  strat_id = 304L,
  fast = 10L,
  slow = 35L,
  signal = 12L
)

macd_contrarian_plot <- make_macd_plot(
  ticker = "XLY",
  asset_label = "Consumer Discretionary Select Sector SPDR Fund",
  from_date = as.Date("2012-01-01"),
  to_date = as.Date("2012-12-31"),
  strategy_label = "MACD contrarian",
  strategy_fun = strategyr::strat_macd_contrarian_tgt_pos,
  strat_id = 305L,
  fast = 10L,
  slow = 35L,
  signal = 7L
)

cat(
  "This example evaluates two MACD-family strategies in separate historical windows because each strategy was selected against a different asset and benchmark. The first plot tests MACD cross on XLU during 2003 with 10/35/12 parameters; positive MACD spread targets trend-following long exposure and negative spread targets short exposure. The second plot tests the named strategyr MACD contrarian strategy on XLY during 2012 with 10/35/7 parameters; it intentionally flips the MACD-cross interpretation, targeting short exposure when MACD spread is positive and long exposure when it is negative. In both plots, pre-window data is used only as signal warmup, while the displayed backtest is restricted to the requested trade window. Each plot compares its evaluated strategy against buy-and-hold for the same asset and same local Yahoo history window.\n\n"
)

print(macd_cross_plot)
print(macd_contrarian_plot)
