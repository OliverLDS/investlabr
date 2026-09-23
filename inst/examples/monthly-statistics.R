transactions <- data.table::data.table(
  datetime = as.POSIXct(c("2025-01-15", "2025-02-15", "2025-03-15"), tz = "UTC"),
  action = "close", pre_fee_log_ret = c(0.02, -0.01, 0.03),
  is_win = c(TRUE, FALSE, TRUE)
)
gen_rolling_monthly_stats(transactions, N_rolling = 2)
