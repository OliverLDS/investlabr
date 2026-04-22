.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("2010-01-01")
assets <- c("SPY", "DX-Y.NYB", "GC=F", "CL=F")
# Optional sync:
# invisible(lapply(assets, function(x) investdatar::sync_local_quantmod_OHLC(x, src = "yahoo")))
# invisible(lapply(c("T10YIE", "UNRATE", "WRBWFRBL"), investdatar::sync_local_fred_data))

reg <- mf_to_wide(list(mf_load_fred("T10YIE", "Inflation expectations", start_date), mf_load_fred("UNRATE", "Unemployment", start_date), mf_load_fred("WRBWFRBL", "Reserves", start_date)))
reg[, regime := fifelse(`Inflation expectations` > median(`Inflation expectations`, na.rm = TRUE) & Unemployment <= median(Unemployment, na.rm = TRUE), "Hot growth", fifelse(Unemployment > median(Unemployment, na.rm = TRUE), "Labor stress", "Benign"))]
current_regime <- reg[.N, regime]

make_asset_fan <- function(symbol) {
  px <- mf_load_yahoo(symbol, symbol, start_date)
  px[, ret := c(NA_real_, diff(log(price)))]
  aligned <- merge(px[, .(date, price, ret)], reg[, .(date, regime)], by = "date", all.x = TRUE)
  aligned[, regime := mf_locf(regime)]
  changes <- aligned[regime == current_regime & is.finite(ret), ret]
  fan <- mf_fan_dt(100, 100 * changes, horizons = c(1L, 5L, 10L, 20L, 60L), seed = nchar(symbol) * 7)
  fan[, asset := symbol]
  fan
}

fan_dt <- rbindlist(lapply(assets, make_asset_fan))
plots <- lapply(assets, function(a) mf_plot_fan(fan_dt[asset == a], a, paste("Conditional on current regime:", current_regime), "Indexed path", "#174A7C"))

board <- investlabr::gen_grid_of_plots_with_labels(
  plots,
  n_rows = 2,
  n_cols = 2,
  title = "Macro-Regime Conditional Cross-Asset Fan Board",
  bottom = "Regimes are simple labels from inflation expectations and unemployment. Fans bootstrap returns observed in the current regime.",
  style = mf_style,
  context = mf_context
)
print(board)
