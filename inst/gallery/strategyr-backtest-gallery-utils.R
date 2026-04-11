build_strategyr_gallery_bt_res <- function(equity, datetime, asset_name, strat_label) {
  investlabr::prep_backtest_result_from_equity(equity, datetime, asset_name, strat_label)
}

run_strategyr_gallery_backtest <- function(
  market_dt,
  tgt_pos,
  strat_id,
  account_leverage,
  contract_size,
  contract_step,
  fee_ratio,
  funding_ratio,
  tol_pos = 0.1
) {
  strategyr::backtest_rcpp(
    timestamp = as.numeric(market_dt$datetime),
    open = market_dt$open,
    high = market_dt$high,
    low = market_dt$low,
    close = market_dt$close,
    tgt_pos = data.table::fifelse(is.na(tgt_pos), 0, tgt_pos),
    pos_strat = rep(as.integer(strat_id), nrow(market_dt)),
    tol_pos = rep(tol_pos, nrow(market_dt)),
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

load_strategyr_gallery_yahoo <- function(ticker, from_date, to_date) {
  market_dt <- data.table::as.data.table(investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo"))
  if (nrow(market_dt) == 0L || !"datetime" %in% names(market_dt)) {
    stop("Local Yahoo OHLC cache is empty or malformed for ticker: ", ticker)
  }
  market_dt <- market_dt[
    datetime >= as.POSIXct(from_date) &
      datetime < as.POSIXct(to_date + 1)
  ][order(datetime)]
  if (nrow(market_dt) < 120L) {
    stop("Need at least 120 rows of local OHLC data for this gallery example: ", ticker)
  }
  market_dt
}

load_strategyr_gallery_yahoo_pair <- function(ticker, benchmark_ticker, from_date, to_date) {
  market_dt <- load_strategyr_gallery_yahoo(ticker, from_date, to_date)
  benchmark_dt <- load_strategyr_gallery_yahoo(benchmark_ticker, from_date, to_date)
  benchmark_close <- benchmark_dt[, .(date = as.Date(datetime), benchmark_close = close)]
  market_dt[, date := as.Date(datetime)]
  market_dt <- merge(market_dt, benchmark_close, by = "date", all = FALSE)[order(datetime)]
  market_dt[, date := NULL]
  if (nrow(market_dt) < 120L) {
    stop(
      "Need at least 120 merged rows for this paired gallery example: ",
      ticker,
      " vs ",
      benchmark_ticker
    )
  }
  market_dt
}

build_strategyr_gallery_ratio_ohlc <- function(numerator_dt, denominator_dt, ratio_symbol) {
  investlabr::prep_ratio_ohlc(numerator_dt, denominator_dt, ratio_symbol)
}
