.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("2005-01-01")
real10 <- mf_load_fred("DFII10", "10Y real yield", start_date)
nom10 <- mf_load_fred("DGS10", "10Y nominal yield", start_date)

make_shock_paths <- function(dt, threshold = 0.25, horizon = 60L) {
  x <- copy(dt)
  x[, chg20 := value - shift(value, 20L)]
  event_dates <- x[abs(chg20) >= threshold, .(event_date = date, sign = fifelse(chg20 > 0, "Up shock", "Down shock"))]
  event_dates <- event_dates[seq(1L, .N, by = 20L)]
  paths <- rbindlist(lapply(seq_len(nrow(event_dates)), function(i) {
    ev <- event_dates[i]
    win <- x[date >= ev$event_date][seq_len(min(.N, horizon + 1L))]
    if (nrow(win) < 10L) return(NULL)
    win[, .(horizon = seq_len(.N) - 1L, value = value - value[1], sign = ev$sign)]
  }), fill = TRUE)
  paths[, .(median = median(value, na.rm = TRUE), p25 = quantile(value, 0.25, na.rm = TRUE), p75 = quantile(value, 0.75, na.rm = TRUE)), by = .(sign, horizon)]
}

real_paths <- make_shock_paths(real10)
nom_paths <- make_shock_paths(nom10)

plot_persist <- function(dt, title) {
  p <- ggplot(dt, aes(horizon, median, color = sign, fill = sign)) +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#7A7A7A") +
    labs(title = title, subtitle = "Median and interquartile post-shock path after 20-day moves above 25bp.", x = "Days after shock", y = "Change since event (pp)", color = NULL, fill = NULL)
  mf_theme(p)
}

p1 <- plot_persist(real_paths, "Real-yield shock persistence")
p2 <- plot_persist(nom_paths, "Nominal-yield shock persistence")
p3 <- mf_plot_lines(real10[date >= Sys.Date() - 365], "date", "value", "series", "Recent 10Y real yield", "Current context for the persistence exercise.", "Real yield (%)")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2, p3),
  n_rows = 1,
  n_cols = 3,
  title = "Real-Yield Persistence Board",
  bottom = "Shock paths are historical analogs, not forecasts. They summarize how real and nominal rates behaved after large 20-day moves.",
  style = mf_style,
  context = mf_context
)
print(board)
