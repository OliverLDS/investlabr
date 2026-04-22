.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("2015-01-01")
# Optional sync:
# invisible(lapply(c("WALCL", "WSHOTSL", "WMBSEC", "WRBWFRBL"), investdatar::sync_local_fred_data))

wide <- mf_to_wide(list(mf_load_fred("WALCL", "Total assets", start_date), mf_load_fred("WSHOTSL", "Treasuries", start_date), mf_load_fred("WMBSEC", "MBS", start_date), mf_load_fred("WRBWFRBL", "Reserves", start_date)))
wide[, (setdiff(names(wide), "date")) := lapply(.SD, function(x) x / 1e6), .SDcols = setdiff(names(wide), "date")]
actual <- melt(wide[date >= Sys.Date() - 365 * 3], id.vars = "date", variable.name = "series", value.name = "value")
latest_total <- wide[.N, `Total assets`]
scenario <- mf_scenario_path(latest_total, c("Current runoff" = -0.06, "Slower runoff" = -0.025, "Stabilization" = 0), horizon_months = 12)

p1 <- mf_plot_lines(actual, "date", "value", "series", "Balance-sheet composition", "Recent total assets and selected components.", "USD tn")
p2 <- mf_plot_lines(scenario, "date", "value", "scenario", "Runoff scenarios", "Alternative total-asset paths over the next year.", "USD tn")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2),
  n_rows = 1,
  n_cols = 2,
  title = "Balance-Sheet Runoff Scenarios",
  bottom = "Runoff assumptions are monthly total-asset changes. This is an explicit scenario board, not a Fed forecast.",
  style = mf_style,
  context = mf_context
)
print(board)
