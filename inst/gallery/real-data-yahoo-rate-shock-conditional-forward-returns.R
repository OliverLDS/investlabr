.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("2010-01-01")
assets <- c("SPY", "DX-Y.NYB", "GC=F", "TLT")
# Optional sync:
# invisible(lapply(assets, function(x) investdatar::sync_local_quantmod_OHLC(x, src = "yahoo")))
# investdatar::sync_local_fred_data("DGS10")

rates <- mf_load_fred("DGS10", "DGS10", start_date)
rates[, shock20 := value - shift(value, 20L)]
events <- rates[abs(shock20) >= 0.30, .(event_date = date, shock = fifelse(shock20 > 0, "Rate up shock", "Rate down shock"))]
events <- events[seq(1L, .N, by = 20L)]

event_paths <- rbindlist(lapply(assets, function(sym) {
  px <- mf_load_yahoo(sym, sym, start_date)
  rbindlist(lapply(seq_len(nrow(events)), function(i) {
    ev <- events[i]
    win <- px[date >= ev$event_date][seq_len(min(.N, 61L))]
    if (nrow(win) < 10L) return(NULL)
    data.table(asset = sym, shock = ev$shock, horizon = seq_len(nrow(win)) - 1L, ret = 100 * (win$price / win$price[1] - 1))
  }), fill = TRUE)
}), fill = TRUE)
summary_dt <- event_paths[, .(median = median(ret, na.rm = TRUE), p25 = quantile(ret, 0.25, na.rm = TRUE), p75 = quantile(ret, 0.75, na.rm = TRUE)), by = .(asset, shock, horizon)]

plots <- lapply(assets, function(a) {
  p <- ggplot(summary_dt[asset == a], aes(horizon, median, color = shock, fill = shock)) +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.16, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#777777") +
    labs(title = a, subtitle = "Forward returns after 20-day 10Y yield shocks.", x = "Days after event", y = "Return (%)", color = NULL, fill = NULL)
  mf_theme(p)
})

board <- investlabr::gen_grid_of_plots_with_labels(
  plots,
  n_rows = 2,
  n_cols = 2,
  title = "Rate-Shock Conditional Forward Returns",
  bottom = "Events are 20-day 10Y Treasury yield moves of at least 30bp. Bands show median and interquartile forward returns.",
  style = mf_style,
  context = mf_context
)
print(board)
