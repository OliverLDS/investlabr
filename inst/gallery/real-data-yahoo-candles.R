library(data.table)
library(investdatar)
library(investlabr)

investdatar::sync_local_quantmod_OHLC(
  ticker = "SPY",
  from = "2024-01-01",
  to = "2024-12-31",
  src = "yahoo"
)

spy_dt <- investdatar::get_local_quantmod_OHLC("SPY", src = "yahoo")
spy_dt <- data.table::as.data.table(spy_dt)

investlabr::gen_candle_plots_with_sr_lines(
  spy_dt[1:60, .(datetime, open, high, low, close)],
  support_pts = c(500, 520),
  resistance_pts = c(560, 580),
  style = "research_note",
  context = "report"
)
