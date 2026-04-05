library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

series_ids <- c("CPIAUCSL", "PCEPI", "T10YIE", "T5YIE", "T5YIFR", "UNRATE", "PAYEMS", "AHETPI")

# Optional if you have not synced locally yet:
# invisible(lapply(series_ids, investdatar::sync_local_fred_data))

get_fred_local <- function(id, label = id) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(id))
  dt[, date := as.Date(date)]
  dt[, value := as.numeric(value)]
  dt[, series := label]
  dt[!is.na(value)]
}

cut_since <- function(dt, from = as.Date("2010-01-01")) {
  dt[date >= from]
}

infl <- rbindlist(list(
  get_fred_local("CPIAUCSL", "CPI"),
  get_fred_local("PCEPI", "PCE")
), use.names = TRUE)
setorder(infl, series, date)
infl[, yoy := (value / shift(value, 12) - 1) * 100, by = series]
infl_yoy <- cut_since(infl[!is.na(yoy)])

p_infl <- ggplot(infl_yoy, aes(x = date, y = yoy, color = series)) +
  geom_hline(yintercept = 2, linetype = "dotted", linewidth = 0.3, color = investlabr::viz_style_get("briefing_serif", "report")$muted) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = investlabr::viz_palette_get("briefing_serif", "report", "discrete")[1:2]) +
  labs(
    title = "CPI and PCE Inflation (YoY)",
    subtitle = "Reference line at 2%",
    x = NULL,
    y = "YoY inflation (%)"
  )
p_infl <- investlabr::viz_theme_apply(
  p_infl,
  style = "briefing_serif",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

breakeven <- rbindlist(list(
  get_fred_local("T10YIE", "10Y breakeven"),
  get_fred_local("T5YIE", "5Y breakeven"),
  get_fred_local("T5YIFR", "5Y5Y forward")
), use.names = TRUE)
breakeven <- cut_since(breakeven)

p_breakeven <- ggplot(breakeven, aes(x = date, y = value, color = series)) +
  geom_hline(yintercept = 2, linetype = "dotted", linewidth = 0.3, color = investlabr::viz_style_get("briefing_serif", "report")$muted) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = investlabr::viz_palette_get("briefing_serif", "report", "discrete")[1:3]) +
  labs(
    title = "Market-Based Inflation Expectations",
    x = NULL,
    y = "Expectation (%)"
  )
p_breakeven <- investlabr::viz_theme_apply(
  p_breakeven,
  style = "briefing_serif",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

unrate <- cut_since(get_fred_local("UNRATE", "Unemployment rate"))
payems <- cut_since(get_fred_local("PAYEMS", "Nonfarm payrolls"))
ahetpi <- cut_since(get_fred_local("AHETPI", "Hourly earnings"))

p_unrate <- ggplot(unrate, aes(x = date, y = value)) +
  geom_line(color = investlabr::viz_style_get("briefing_serif", "report")$accent, linewidth = 0.5) +
  labs(title = "Unemployment Rate", x = NULL, y = "Unemployment (%)")
p_unrate <- investlabr::viz_theme_apply(
  p_unrate,
  style = "briefing_serif",
  context = "report",
  show_compiler = FALSE
)

p_payrolls <- ggplot(payems, aes(x = date, y = value / 1000)) +
  geom_line(color = investlabr::viz_style_get("briefing_serif", "report")$accent2, linewidth = 0.5) +
  labs(title = "Nonfarm Payrolls", x = NULL, y = "Millions")
p_payrolls <- investlabr::viz_theme_apply(
  p_payrolls,
  style = "briefing_serif",
  context = "report",
  show_compiler = FALSE
)

p_wage <- ggplot(ahetpi, aes(x = date, y = value)) +
  geom_line(color = investlabr::viz_style_get("briefing_serif", "report")$resistance, linewidth = 0.5) +
  labs(title = "Hourly Earnings", x = NULL, y = "USD")
p_wage <- investlabr::viz_theme_apply(
  p_wage,
  style = "briefing_serif",
  context = "report",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(
    p_infl, p_breakeven,
    p_unrate, p_payrolls,
    p_wage, NULL
  ),
  n_rows = 3,
  n_cols = 2,
  title = "Inflation and Labor Dashboard",
  bottom = "FRED inputs: CPIAUCSL, PCEPI, T10YIE, T5YIE, T5YIFR, UNRATE, PAYEMS, AHETPI.",
  style = "briefing_serif",
  context = "report"
)
