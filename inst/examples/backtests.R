prices <- data.table::data.table(
  datetime = as.POSIXct("2025-01-01", tz = "UTC") + 86400 * 0:39,
  open = 100 + (0:39) / 10,
  close = 100 + (0:39) / 10 + sin(0:39),
  position = rep(c(0, 1), each = 5, length.out = 40)
)
attr(prices, "inst_id") <- "Synthetic asset"
attr(prices$position, "strat_name") <- "Illustration"
attr(prices$position, "strat_par") <- list(window = 5)
result <- eval_strat_performance(prices, "position", tz = "UTC")
eval_strat_plot_tsline_eq(result)
eval_strat_plot_scatter_maxdd_annret(result)

# An external backtest can instead supply an equity curve.
adapted <- prep_backtest_result_from_equity(
  exp(cumsum(0.001 + 0.003 * sin(0:39))), prices$datetime,
  "Synthetic asset", "Illustrative equity curve"
)
adapted[, c("asset_name", "annual_return", "max_drawdown")]
