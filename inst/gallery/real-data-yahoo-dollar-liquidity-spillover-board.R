.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("2010-01-01")
assets <- c("SPY", "DX-Y.NYB", "GC=F", "HYG")
# Optional sync:
# invisible(lapply(assets, function(x) investdatar::sync_local_quantmod_OHLC(x, src = "yahoo")))
# invisible(lapply(c("SOFR", "IORB", "WRBWFRBL"), investdatar::sync_local_fred_data))

liq <- mf_to_wide(list(mf_load_fred("SOFR", "SOFR", start_date), mf_load_fred("IORB", "IORB", start_date), mf_load_fred("WRBWFRBL", "Reserves", start_date)))
liq[, stress := mf_zscore(100 * (SOFR - IORB)) + mf_zscore(-(Reserves - shift(Reserves, 20L)))]
liq[, episode := fifelse(stress > stats::quantile(stress, 0.8, na.rm = TRUE), "Tight dollar liquidity", fifelse(stress < stats::quantile(stress, 0.2, na.rm = TRUE), "Easing liquidity", NA_character_))]
events <- liq[!is.na(episode), .(event_date = date, episode)]
events <- events[seq(1L, .N, by = 20L)]

paths <- rbindlist(lapply(assets, function(sym) {
  px <- mf_load_yahoo(sym, sym, start_date)
  rbindlist(lapply(seq_len(nrow(events)), function(i) {
    ev <- events[i]
    win <- px[date >= ev$event_date][seq_len(min(.N, 41L))]
    if (nrow(win) < 10L) return(NULL)
    data.table(asset = sym, episode = ev$episode, horizon = seq_len(nrow(win)) - 1L, ret = 100 * (win$price / win$price[1] - 1))
  }), fill = TRUE)
}), fill = TRUE)
summary_dt <- paths[, .(median = median(ret, na.rm = TRUE)), by = .(asset, episode, horizon)]

plots <- lapply(assets, function(a) {
  p <- ggplot(summary_dt[asset == a], aes(horizon, median, color = episode)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#777777") +
    labs(title = a, subtitle = "Median path after dollar-liquidity stress or easing episodes.", x = "Days after event", y = "Return (%)", color = NULL)
  mf_theme(p)
})

board <- investlabr::gen_grid_of_plots_with_labels(
  plots,
  n_rows = 2,
  n_cols = 2,
  title = "Dollar Liquidity Spillover Board",
  bottom = "Episodes are heuristic SOFR-IORB and reserve-drain stress states. Paths summarize conditional cross-asset spillovers.",
  style = mf_style,
  context = mf_context
)
print(board)
