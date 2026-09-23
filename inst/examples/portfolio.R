datetime <- as.POSIXct("2025-01-01", tz = "UTC") + 86400 * 0:59
results <- data.table::rbindlist(lapply(1:3, function(i) {
  equity <- exp(cumsum(0.0005 * i + 0.004 * sin((1:60) * i)))
  prep_backtest_result_from_equity(equity, datetime, paste("Asset", i), "Hold")
}))
# Choose a feasible target from the actual synthetic results.
target <- min(results$annual_return)
get_optimal_weights(results, target_return = target)
eval_portfolio_performance(results, target_return = target)
