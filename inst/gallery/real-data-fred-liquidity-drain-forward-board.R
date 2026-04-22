.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- Sys.Date() - 365
# Optional sync:
# invisible(lapply(c("WRBWFRBL", "WDTGAL", "WLRRAL"), investdatar::sync_local_fred_data))

wide <- mf_to_wide(list(mf_load_fred("WRBWFRBL", "Reserves", start_date), mf_load_fred("WDTGAL", "TGA", start_date), mf_load_fred("WLRRAL", "Reverse repos", start_date)))
wide[, `:=`(Reserves = Reserves / 1e6, TGA = TGA / 1e6, `Reverse repos` = `Reverse repos` / 1e6)]
actual <- melt(wide, id.vars = "date", variable.name = "series", value.name = "value")
latest_res <- wide[.N, Reserves]
scenario <- mf_scenario_path(latest_res, c("TGA rebuild drain" = -0.08, "RRP runoff cushion" = 0.03, "Neutral drift" = -0.02), horizon_months = 6)

p1 <- mf_plot_lines(actual[date >= Sys.Date() - 180], "date", "value", "series", "Recent liquidity plumbing", "Reserves, TGA, and reverse repos in USD trillions.", "USD tn")
p2 <- mf_plot_lines(scenario, "date", "value", "scenario", "Forward reserve drain scenarios", "Simple accounting-style monthly reserve changes.", "Reserve balances (USD tn)")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2),
  n_rows = 1,
  n_cols = 2,
  title = "Liquidity Drain Forward Board",
  bottom = "Assumptions are monthly reserve changes: TGA rebuild drain -$80bn, RRP runoff cushion +$30bn, neutral drift -$20bn.",
  style = mf_style,
  context = mf_context
)
print(board)
