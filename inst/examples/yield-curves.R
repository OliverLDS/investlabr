series <- lapply(seq_len(11), function(i) {
  data.table::data.table(date = as.Date(c("2025-01-01", "2025-02-01")),
                         value = c(4 + i / 20, 3.8 + i / 20))
})
curves <- get_yield_data_DT(series, list(
  Now = as.Date("2025-02-01"), Previous = as.Date("2025-01-01")
))
gen_yield_curve_plot(curves, selected_windows = c("Now", "Previous"))
gen_yield_curve_plot_grid(curves, selected_windows = c("Now", "Previous"))
