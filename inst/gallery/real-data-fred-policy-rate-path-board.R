.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- Sys.Date() - 365

# Optional sync:
# invisible(lapply(c("EFFR", "SOFR", "IORB", "DFEDTARL", "DFEDTARU"), investdatar::sync_local_fred_data))

policy <- mf_to_wide(list(
  mf_load_fred("EFFR", "EFFR", start_date),
  mf_load_fred("SOFR", "SOFR", start_date),
  mf_load_fred("IORB", "IORB", start_date),
  mf_load_fred("DFEDTARL", "Fed target lower", start_date),
  mf_load_fred("DFEDTARU", "Fed target upper", start_date)
))
recent <- policy[date >= Sys.Date() - 120]
recent_long <- melt(recent, id.vars = "date", measure.vars = c("EFFR", "SOFR", "IORB"), variable.name = "series", value.name = "value")
latest_iorb <- tail(policy$IORB[is.finite(policy$IORB)], 1)
scenario_dt <- mf_scenario_path(latest_iorb, c("Sticky restrictive" = 0.00, "Gradual normalization" = -0.25, "Renewed tightening" = 0.125), horizon_months = 6)
spread_dt <- recent[, .(date, `EFFR - IORB` = 100 * (EFFR - IORB), `SOFR - IORB` = 100 * (SOFR - IORB))]
spread_long <- melt(spread_dt, id.vars = "date", variable.name = "series", value.name = "bps")

p1 <- mf_plot_lines(recent_long, "date", "value", "series", "Recent policy-rate complex", "EFFR, SOFR, and IORB over the last four months.", "Rate (%)")
p2 <- mf_plot_lines(scenario_dt, "date", "value", "scenario", "Six-month policy-rate scenarios", "Simple deterministic paths from the latest IORB level.", "Rate (%)")
p3 <- mf_plot_lines(spread_long, "date", "bps", "series", "Implementation spreads", "Positive spreads imply market rates are rich to IORB.", "Basis points")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2, p3),
  n_rows = 1,
  n_cols = 3,
  title = "Policy Rate Path Board",
  bottom = "Scenarios are transparent assumptions: no-cut sticky path, gradual normalization, and renewed tightening. FRED inputs: EFFR, SOFR, IORB, DFEDTARL, DFEDTARU.",
  style = mf_style,
  context = mf_context
)
print(board)
