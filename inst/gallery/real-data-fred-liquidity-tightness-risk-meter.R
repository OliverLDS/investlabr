.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- Sys.Date() - 365 * 3
# Optional sync:
# invisible(lapply(c("EFFR", "SOFR", "IORB", "WRBWFRBL", "DTB4WK"), investdatar::sync_local_fred_data))

wide <- mf_to_wide(list(mf_load_fred("EFFR", "EFFR", start_date), mf_load_fred("SOFR", "SOFR", start_date), mf_load_fred("IORB", "IORB", start_date), mf_load_fred("WRBWFRBL", "Reserves", start_date), mf_load_fred("DTB4WK", "4W bill", start_date)))
wide[, `:=`(floor_pressure = rowMeans(.SD, na.rm = TRUE), reserve_drain = -(Reserves - shift(Reserves, 13L)) / 1e6, bill_pressure = 100 * (`4W bill` - IORB)), .SDcols = c("EFFR", "SOFR")]
wide[, floor_pressure := 100 * (floor_pressure - IORB)]
wide[, score := mf_bound_score(rowMeans(.SD, na.rm = TRUE)), .SDcols = c("floor_pressure", "reserve_drain", "bill_pressure")]
recent <- wide[date >= Sys.Date() - 365]
component <- melt(recent[, .(date, floor_pressure = mf_zscore(floor_pressure), reserve_drain = mf_zscore(reserve_drain), bill_pressure = mf_zscore(bill_pressure))], id.vars = "date")

p1 <- ggplot(recent, aes(date, score)) + geom_hline(yintercept = c(40, 70), linetype = "dotted", color = "#777777") + geom_line(color = "#B24C45", linewidth = 0.9) + labs(title = "Liquidity tightness risk meter", subtitle = mf_latest_text(tail(recent$score, 1)), x = NULL, y = "Score (0-100)")
p1 <- mf_theme(p1, "none")
p2 <- mf_plot_lines(component, "date", "value", "variable", "Meter components", "Window z-scores for funding, reserve drain, and bill pressure.", "z-score")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2),
  n_rows = 1,
  n_cols = 2,
  title = "Liquidity Tightness Risk Meter",
  bottom = "The meter is a bounded heuristic score built from floor pressure, reserve drain, and bill pressure. It is forward-looking as a near-term risk gauge, not a calibrated probability.",
  style = mf_style,
  context = mf_context
)
print(board)
