.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("2000-01-01")
recent_window <- 252L
horizons <- c(1L, 5L, 10L, 20L, 60L)

# Optional sync:
# invisible(lapply(c("DGS2", "DGS10", "DGS30"), investdatar::sync_local_fred_data))

rates <- mf_to_wide(list(
  mf_load_fred("DGS2", "DGS2", start_date),
  mf_load_fred("DGS10", "DGS10", start_date),
  mf_load_fred("DGS30", "DGS30", start_date)
))
rates[, `10Y-2Y` := DGS10 - DGS2]

fan_2y <- mf_fan_dt(tail(rates$DGS2[is.finite(rates$DGS2)], 1), mf_recent_changes(rates$DGS2, recent_window), horizons, seed = 11)
fan_10y <- mf_fan_dt(tail(rates$DGS10[is.finite(rates$DGS10)], 1), mf_recent_changes(rates$DGS10, recent_window), horizons, seed = 12)
fan_slope <- mf_fan_dt(tail(rates$`10Y-2Y`[is.finite(rates$`10Y-2Y`)], 1), mf_recent_changes(rates$`10Y-2Y`, recent_window), horizons, seed = 13)

p1 <- mf_plot_fan(fan_2y, "2Y Treasury forward fan", "Recent-regime daily changes bootstrapped into forward percentiles.", "Yield (%)", "#174A7C")
p2 <- mf_plot_fan(fan_10y, "10Y Treasury forward fan", "Same method applied to the long-rate anchor.", "Yield (%)", "#B23A48")
p3 <- mf_plot_fan(fan_slope, "10Y-2Y slope forward fan", "Slope fan shows the distribution of curve steepening or flattening.", "Spread (pp)", "#2A7F62")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2, p3),
  n_rows = 1,
  n_cols = 3,
  title = "Treasury Curve Forward Fan",
  bottom = paste("Fans use bootstrapped daily changes from the last", recent_window, "observations. They are scenario distributions, not point forecasts."),
  style = mf_style,
  context = mf_context
)
print(board)
