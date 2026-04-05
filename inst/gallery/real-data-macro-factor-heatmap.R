library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

tickers <- c(
  "AAPL", "GOOG", "BMNR", "MSTR", "COIN", "TSLA", "PLTR", "ORCL",
  "NVDA", "AVGO", "ASML", "AMZN", "MSFT", "AMD", "IBM"
)

fred_series <- c("SP500", "NASDAQCOM", "DFII10", "SOFR", "IORB", "VIXCLS", "STLFSI2", "WRBWFRBL")

start_date <- as.Date("2015-01-01")
end_date <- Sys.Date()
min_obs_full <- 252L

# Optional if you have not synced locally yet:
# invisible(lapply(
#   tickers,
#   function(tk) investdatar::sync_local_quantmod_OHLC(
#     ticker = tk, label = tk, from = start_date, to = end_date, src = "yahoo"
#   )
# ))
# invisible(lapply(fred_series, investdatar::sync_local_fred_data))

`%||%` <- function(a, b) if (!is.null(a)) a else b

zscore <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sdv <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mu) / sdv
}

get_stock_price_dt <- function(ticker) {
  dt <- tryCatch(
    data.table::as.data.table(investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo")),
    error = function(e) NULL
  )

  if (!is.null(dt) && nrow(dt) > 0L) {
    price_col <- if ("adj_close" %in% names(dt) && any(!is.na(dt$adj_close))) "adj_close" else "close"
    out <- dt[, .(date = as.Date(date), price = as.numeric(get(price_col)))]
    out <- out[date >= start_date & date <= end_date & is.finite(price)]
    if (nrow(out) > 0L) return(out)
  }

  legacy_path <- file.path(investdatar::get_source_data_path("yahoofinance", create = FALSE), paste0(ticker, ".rds"))
  if (!file.exists(legacy_path)) return(NULL)

  legacy_dt <- tryCatch(data.table::as.data.table(readRDS(legacy_path)), error = function(e) NULL)
  if (is.null(legacy_dt) || nrow(legacy_dt) == 0L) return(NULL)

  date_col <- names(legacy_dt)[tolower(names(legacy_dt)) == "date"][1]
  close_col <- names(legacy_dt)[tolower(names(legacy_dt)) == "close"][1]
  if (is.na(date_col) || is.na(close_col)) return(NULL)

  out <- legacy_dt[, .(
    date = as.Date(get(date_col)),
    price = as.numeric(get(close_col))
  )]
  out[date >= start_date & date <= end_date & is.finite(price)]
}

get_fred_dt <- function(series_id) {
  dt <- copy(investdatar::get_local_FRED_data(series_id))
  dt <- dt[date >= start_date & date <= end_date & !is.na(value), .(date, value = as.numeric(value))]
  setorder(dt, date)
  dt
}

sp500_dt <- get_fred_dt("SP500")
nasdaq_dt <- get_fred_dt("NASDAQCOM")
dfii10_dt <- get_fred_dt("DFII10")
sofr_dt <- get_fred_dt("SOFR")
iorb_dt <- get_fred_dt("IORB")
vix_dt <- get_fred_dt("VIXCLS")
stlfsi_dt <- get_fred_dt("STLFSI2")
reserves_dt <- get_fred_dt("WRBWFRBL")

sp500_dt[, mkt_sp500_ret := c(NA_real_, diff(log(value)))]
nasdaq_dt[, nasdaq_ret := c(NA_real_, diff(log(value)))]
market_dt <- merge(
  sp500_dt[, .(date, mkt_sp500_ret)],
  nasdaq_dt[, .(date, nasdaq_ret)],
  by = "date",
  all = TRUE
)
setorder(market_dt, date)
market_dt[, c("mkt_sp500_ret", "nasdaq_ret") := lapply(.SD, nafill, type = "locf"), .SDcols = c("mkt_sp500_ret", "nasdaq_ret")]
market_dt[, nasdaq_rel_ret := nasdaq_ret - mkt_sp500_ret]

liquidity_dt <- merge(sofr_dt, iorb_dt, by = "date", all = TRUE, suffixes = c("_sofr", "_iorb"))
setorder(liquidity_dt, date)
liquidity_dt[, c("value_sofr", "value_iorb") := lapply(.SD, nafill, type = "locf"), .SDcols = c("value_sofr", "value_iorb")]
liquidity_dt[, liq_sofr_iorb_chg := c(NA_real_, diff(value_sofr - value_iorb))]

dfii10_dt[, real10y_chg := c(NA_real_, diff(value))]
vix_dt[, vix_chg := c(NA_real_, diff(value))]
stlfsi_dt[, stlfsi_chg := c(NA_real_, diff(value))]
reserves_dt[, reserve_log_chg := c(NA_real_, diff(log(value)))]

factor_dt <- Reduce(
  function(x, y) merge(x, y, by = "date", all = TRUE),
  list(
    market_dt[, .(date, mkt_sp500_ret, nasdaq_rel_ret)],
    dfii10_dt[, .(date, real10y_chg)],
    liquidity_dt[, .(date, liq_sofr_iorb_chg)],
    vix_dt[, .(date, vix_chg)],
    stlfsi_dt[, .(date, stlfsi_chg)],
    reserves_dt[, .(date, reserve_log_chg)]
  )
)
setorder(factor_dt, date)
factor_cols <- setdiff(names(factor_dt), "date")
factor_dt[, (factor_cols) := lapply(.SD, nafill, type = "locf"), .SDcols = factor_cols]
factor_dt <- factor_dt[complete.cases(factor_dt)]
factor_dt[, (factor_cols) := lapply(.SD, zscore), .SDcols = factor_cols]

estimate_one_ticker <- function(ticker) {
  px <- get_stock_price_dt(ticker)
  if (is.null(px) || nrow(px) < (min_obs_full + 20L)) return(NULL)
  setorder(px, date)
  px[, stock_ret := c(NA_real_, diff(log(price)))]
  px <- px[!is.na(stock_ret), .(date, stock_ret = zscore(stock_ret))]
  aligned_dt <- merge(px, factor_dt, by = "date", all = FALSE)
  aligned_dt <- aligned_dt[complete.cases(aligned_dt)]
  if (nrow(aligned_dt) < min_obs_full) return(NULL)

  fit <- lm(
    stock_ret ~ mkt_sp500_ret + nasdaq_rel_ret + real10y_chg +
      liq_sofr_iorb_chg + vix_chg + stlfsi_chg + reserve_log_chg,
    data = aligned_dt
  )
  coef_dt <- as.data.table(summary(fit)$coefficients, keep.rownames = "term")
  setnames(coef_dt, c("term", "estimate", "std_error", "t_value", "p_value"))
  coef_dt <- coef_dt[term != "(Intercept)"]
  coef_dt[, `:=`(
    ticker = ticker,
    n_obs = nrow(aligned_dt)
  )]
  coef_dt
}

beta_dt <- rbindlist(lapply(tickers, estimate_one_ticker), fill = TRUE)
if (nrow(beta_dt) == 0L) {
  stop("No locally available stock series produced enough observations for the heatmap. Sync the requested Yahoo tickers first.", call. = FALSE)
}

factor_labels <- c(
  "mkt_sp500_ret" = "S&P 500 return",
  "nasdaq_rel_ret" = "Nasdaq minus S&P 500",
  "real10y_chg" = "10y real yield change",
  "liq_sofr_iorb_chg" = "SOFR - IORB change",
  "vix_chg" = "VIX change",
  "stlfsi_chg" = "Financial stress change",
  "reserve_log_chg" = "Reserve balances growth"
)

beta_dt[, factor_label := factor_labels[term]]
beta_dt[, star := fifelse(p_value < 0.01, "**", fifelse(p_value < 0.05, "*", ""))]

ticker_order <- beta_dt[term == "mkt_sp500_ret"][order(-estimate), ticker]
beta_dt[, ticker := factor(ticker, levels = ticker_order)]
beta_dt[, factor_label := factor(
  factor_label,
  levels = factor_labels
)]

included_tickers <- unique(as.character(beta_dt$ticker))
avg_obs <- round(mean(unique(beta_dt[, .(ticker, n_obs)])$n_obs))

p <- ggplot(beta_dt, aes(x = factor_label, y = ticker, fill = estimate)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = star), size = 4.2, color = "black") +
  scale_fill_gradient2(
    low = "#B35C44",
    mid = "#F8F8F4",
    high = "#355C7D",
    midpoint = 0
  ) +
  labs(
    title = "Macro factor sensitivity heatmap",
    subtitle = paste0(
      "Standardized full-sample betas from ", format(start_date, "%Y-%m-%d"),
      " to ", format(end_date, "%Y-%m-%d"),
      " | * p < 0.05, ** p < 0.01"
    ),
    x = NULL,
    y = NULL,
    fill = "Beta",
    caption = paste(
      "Factors: S&P 500 return, Nasdaq relative return, 10y real yield change, SOFR-IORB change, VIX change, financial stress change, reserve-balances growth.",
      paste0("Included stocks: ", paste(included_tickers, collapse = ", "), "."),
      paste0("Average aligned observations per stock: ", avg_obs, ".")
    )
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

p <- investlabr::viz_theme_apply(
  p,
  style = "institutional_blue",
  context = "report",
  legend_position = "right",
  show_compiler = TRUE
)

print(p)
