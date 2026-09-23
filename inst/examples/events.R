prices <- data.table::data.table(
  datetime = as.POSIXct("2025-01-01", tz = "UTC") + 86400 * 0:59,
  close = 100 * exp(0.001 * (0:59) + 0.01 * sin(0:59)),
  event_a = as.integer(seq_len(60) %% 10 == 0),
  event_b = as.integer(seq_len(60) %% 12 == 0)
)
attr(prices, "inst_id") <- "Synthetic asset"
a <- eval_event_performance(prices, "event_a", H = 1:5)
b <- eval_event_performance(prices, "event_b", H = 1:5)
gen_plot_event_tsline_cum_ret(a)
gen_plot_comparing_events(a, b, "Event A", "Event B")
