.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("2010-01-01")
# Optional sync:
# invisible(lapply(c("CPIAUCSL", "CPILFESL", "AHETPI", "T5YIFR"), investdatar::sync_local_fred_data))

wide <- mf_to_wide(Filter(Negate(is.null), list(
  mf_load_fred("CPIAUCSL", "CPI", start_date),
  mf_try_fred("CPILFESL", "Core CPI", start_date),
  mf_try_fred("AHETPI", "Hourly earnings", start_date),
  mf_try_fred("T5YIFR", "5Y5Y", start_date)
)))
wide[, cpi_yoy := 100 * (CPI / shift(CPI, 12L) - 1)]
if ("Core CPI" %in% names(wide)) wide[, core_yoy := 100 * (`Core CPI` / shift(`Core CPI`, 12L) - 1)]
if ("Hourly earnings" %in% names(wide)) wide[, wage_yoy := 100 * (`Hourly earnings` / shift(`Hourly earnings`, 12L) - 1)]
indicator_map <- c(cpi_yoy = "CPI")
if ("core_yoy" %in% names(wide)) indicator_map <- c(indicator_map, core_yoy = "Core CPI")
if ("wage_yoy" %in% names(wide)) indicator_map <- c(indicator_map, wage_yoy = "Wage growth")
latest <- wide[.N]
scenario_steps <- data.table(
  variable = names(indicator_map),
  series = unname(indicator_map),
  Disinflation = c(cpi_yoy = -0.18, core_yoy = -0.12, wage_yoy = -0.08)[names(indicator_map)],
  Reacceleration = c(cpi_yoy = 0.16, core_yoy = 0.10, wage_yoy = 0.06)[names(indicator_map)]
)
scenarios <- c("Disinflation", "Reacceleration")
scenario_dt <- rbindlist(lapply(scenarios, function(s) {
  dates <- seq(Sys.Date(), by = "month", length.out = 13L)
  rbindlist(lapply(seq_len(nrow(scenario_steps)), function(i) {
    var <- scenario_steps$variable[i]
    data.table(
      date = dates,
      scenario = s,
      series = scenario_steps$series[i],
      value = latest[[var]] + c(0, cumsum(rep(scenario_steps[[s]][i], 12L)))
    )
  }))
}))
recent <- melt(
  wide[date >= Sys.Date() - 730, c("date", names(indicator_map)), with = FALSE],
  id.vars = "date",
  variable.name = "variable",
  value.name = "value"
)
recent[, variable := indicator_map[as.character(variable)]]

p1 <- mf_plot_lines(recent[!is.na(value)], "date", "value", "variable", "Recent inflation and wage pressure", "Starting point for the scenario paths.", "YoY (%)")
p2 <- mf_plot_lines(scenario_dt, "date", "value", "scenario", "Disinflation versus reacceleration paths", "Explicit monthly assumptions applied to latest values.", "YoY (%)")
p2 <- p2 + facet_wrap(~series, ncol = 1, scales = "free_y")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2),
  n_rows = 1,
  n_cols = 2,
  title = "Disinflation vs Reacceleration Scenarios",
  bottom = "Scenario paths are deterministic assumptions for CPI, core CPI, and wages. They are designed for briefing comparison, not forecast precision.",
  style = mf_style,
  context = mf_context
)
print(board)
