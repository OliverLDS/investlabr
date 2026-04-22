.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("1990-01-01")
# Optional sync:
# invisible(lapply(c("UNRATE", "PAYEMS", "CCSA", "JTSQUR"), investdatar::sync_local_fred_data))

wide <- mf_to_wide(list(
  mf_load_fred("UNRATE", "UNRATE", start_date),
  mf_load_fred("PAYEMS", "PAYEMS", start_date),
  mf_load_fred("CCSA", "CCSA", start_date),
  mf_load_fred("JTSQUR", "JTSQUR", start_date)
))
wide[, payroll_3m := data.table::frollmean(PAYEMS - shift(PAYEMS), 3L, align = "right", na.rm = TRUE)]
wide[, `:=`(
  unrate_score = mf_zscore(UNRATE - shift(UNRATE, 6L)),
  claims_score = mf_zscore(log(CCSA) - shift(log(CCSA), 13L)),
  payroll_score = mf_zscore(-payroll_3m),
  quits_score = mf_zscore(-JTSQUR)
)]
wide[, softening_score := mf_bound_score(rowMeans(.SD, na.rm = TRUE)), .SDcols = c("unrate_score", "claims_score", "payroll_score", "quits_score")]
recent <- wide[date >= Sys.Date() - 730]
component <- melt(recent[, .(date, unrate_score, claims_score, payroll_score, quits_score)], id.vars = "date", variable.name = "series", value.name = "z")
component[, series := factor(series, labels = c("Unemployment impulse", "Continuing claims impulse", "Payroll slowdown", "Lower quits"))]

p1 <- ggplot(recent, aes(date, softening_score)) + geom_hline(yintercept = c(40, 70), linetype = "dotted", color = "#777777") + geom_line(color = "#B24C45", linewidth = 0.9) + labs(title = "Labor softening risk score", subtitle = mf_latest_text(tail(recent$softening_score, 1)), x = NULL, y = "Score (0-100)")
p1 <- mf_theme(p1, "none")
p2 <- mf_plot_lines(component, "date", "z", "series", "Component pressure", "Positive z-scores indicate labor-market softening pressure.", "Window z-score")
p3 <- mf_plot_lines(recent[, .(date, `Unemployment rate` = UNRATE, `Payroll gains, 3m avg` = payroll_3m)] |> melt(id.vars = "date"), "date", "value", "variable", "Current labor pulse", "Levels and momentum behind the score.", "Value")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2, p3),
  n_rows = 1,
  n_cols = 3,
  title = "Labor Softening Probability-Style Board",
  bottom = "This is a heuristic bounded risk score, not a formally calibrated recession probability. Inputs: UNRATE, PAYEMS, CCSA, JTSQUR.",
  style = mf_style,
  context = mf_context
)
print(board)
