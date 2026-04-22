.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("1990-01-01")
# Optional sync:
# invisible(lapply(c("CPIAUCSL", "UNRATE", "T10Y2Y", "BAMLH0A0HYM2"), investdatar::sync_local_fred_data))

wide <- mf_to_wide(list(mf_load_fred("CPIAUCSL", "CPI", start_date), mf_load_fred("UNRATE", "Unemployment", start_date), mf_load_fred("T10Y2Y", "Curve slope", start_date), mf_load_fred("BAMLH0A0HYM2", "HY spread", start_date)))
wide[, inflation := 100 * (CPI / shift(CPI, 12L) - 1)]
wide[, landing_type := fifelse(inflation < 3 & Unemployment < 5 & `HY spread` < 5, "Soft-landing template", fifelse(Unemployment > 6 | `HY spread` > 7, "Hard-landing template", NA_character_))]
current <- wide[.N, .(inflation, Unemployment, `Curve slope`, `HY spread`)]
templates <- wide[!is.na(landing_type), .(inflation = median(inflation, na.rm = TRUE), Unemployment = median(Unemployment, na.rm = TRUE), `Curve slope` = median(`Curve slope`, na.rm = TRUE), `HY spread` = median(`HY spread`, na.rm = TRUE)), by = landing_type]
plot_dt <- melt(rbindlist(list(data.table(landing_type = "Current", current), templates), fill = TRUE), id.vars = "landing_type", variable.name = "indicator", value.name = "value")

p1 <- ggplot(plot_dt, aes(indicator, value, fill = landing_type)) + geom_col(position = "dodge") + labs(title = "Current state versus landing templates", subtitle = "Simple historical templates contrast benign and stressed macro states.", x = NULL, y = "Value", fill = NULL)
p1 <- mf_theme(p1)
p2 <- mf_plot_lines(melt(wide[date >= Sys.Date() - 365 * 3, .(date, inflation, Unemployment, `HY spread`)], id.vars = "date"), "date", "value", "variable", "Recent path into the landing debate", "Inflation, labor, and credit context.", "Value")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2),
  n_rows = 1,
  n_cols = 2,
  title = "Soft Landing vs Hard Landing Board",
  bottom = "Templates are simple historical state summaries. The board positions current macro conditions against those templates for briefing use.",
  style = mf_style,
  context = mf_context
)
print(board)
