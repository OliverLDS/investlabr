.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("2000-01-01")
# Optional sync:
# invisible(lapply(c("CPIAUCSL", "CPILFESL", "PCEPI"), investdatar::sync_local_fred_data))

cpi <- mf_load_fred("CPIAUCSL", "Headline CPI", start_date)
core <- mf_try_fred("CPILFESL", "Core CPI", start_date)
pce <- mf_try_fred("PCEPI", "PCE", start_date)
infl <- rbindlist(Filter(Negate(is.null), list(cpi, core, pce)), fill = TRUE)
infl[, mom := 100 * (value / shift(value) - 1), by = series]
infl[, yoy := 100 * (value / shift(value, 12L) - 1), by = series]
recent <- infl[date >= Sys.Date() - 730]

latest <- cpi[.N, value]
mom_hist <- tail(cpi[, 100 * (value / shift(value) - 1)], 120)
fan <- mf_fan_dt(latest, latest * mom_hist[is.finite(mom_hist)] / 100, horizons = 1:6, n_paths = 800, seed = 22)
fan[, `:=`(p10 = 100 * (p10 / latest - 1), p25 = 100 * (p25 / latest - 1), p50 = 100 * (p50 / latest - 1), p75 = 100 * (p75 / latest - 1), p90 = 100 * (p90 / latest - 1))]

p1 <- mf_plot_lines(recent[!is.na(yoy)], "date", "yoy", "series", "Recent inflation history", "Headline, core, and PCE year-over-year inflation.", "YoY (%)")
p2 <- mf_plot_fan(fan, "Headline CPI nowcast band", "Six monthly release fan based on the recent 10-year monthly change distribution.", "Cumulative change from latest CPI (%)", "#B85C38")
p3 <- mf_plot_lines(recent[!is.na(mom)], "date", "mom", "series", "Monthly inflation momentum", "Monthly percent changes show near-term inflation impulse.", "MoM (%)")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2, p3),
  n_rows = 1,
  n_cols = 3,
  title = "Inflation Nowcast Band Board",
  bottom = "The CPI nowcast band bootstraps recent monthly CPI changes. It is a transparent range estimate, not a formal econometric nowcast.",
  style = mf_style,
  context = mf_context
)
print(board)
