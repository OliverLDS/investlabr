.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("1990-01-01")
# Optional sync:
# invisible(lapply(c("T10Y2Y", "UNRATE", "CCSA", "BAMLH0A0HYM2"), investdatar::sync_local_fred_data))

wide <- mf_to_wide(list(mf_load_fred("T10Y2Y", "Curve slope", start_date), mf_load_fred("UNRATE", "Unemployment", start_date), mf_load_fred("CCSA", "Continuing claims", start_date), mf_load_fred("BAMLH0A0HYM2", "HY spread", start_date)))
wide[, `:=`(curve_score = mf_zscore(-`Curve slope`), unrate_score = mf_zscore(Unemployment - shift(Unemployment, 6L)), claims_score = mf_zscore(log(`Continuing claims`) - shift(log(`Continuing claims`), 13L)), credit_score = mf_zscore(`HY spread`))]
wide[, recession_score := mf_bound_score(rowMeans(.SD, na.rm = TRUE)), .SDcols = c("curve_score", "unrate_score", "claims_score", "credit_score")]
recent <- wide[date >= Sys.Date() - 365 * 5]
component <- melt(recent[, .(date, curve_score, unrate_score, claims_score, credit_score)], id.vars = "date", variable.name = "series", value.name = "z")

p1 <- ggplot(recent, aes(date, recession_score)) + geom_hline(yintercept = c(40, 70), linetype = "dotted", color = "#777777") + geom_line(color = "#B24C45", linewidth = 0.9) + labs(title = "Recession probability-style score", subtitle = mf_latest_text(tail(recent$recession_score, 1)), x = NULL, y = "Score (0-100)")
p1 <- mf_theme(p1, "none")
p2 <- mf_plot_lines(component, "date", "z", "series", "Drivers", "Higher z-scores indicate more recession-risk pressure.", "z-score")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2),
  n_rows = 1,
  n_cols = 2,
  title = "Recession Probability Dashboard",
  bottom = "This is a heuristic probability-style score from curve inversion, labor deterioration, claims, and credit spreads, not a formally calibrated recession model.",
  style = mf_style,
  context = mf_context
)
print(board)
