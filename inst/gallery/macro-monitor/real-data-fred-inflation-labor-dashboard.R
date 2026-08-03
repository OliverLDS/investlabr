library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

series_ids <- c("CPIAUCSL", "PCEPI", "T5YIE", "T5YIFR", "UNRATE", "PAYEMS", "AHETPI")
start_date <- as.Date("2010-01-01")
style_name <- "briefing_serif"
context_name <- "report"
style <- investlabr::viz_style_get(style_name, context_name)
palette <- investlabr::viz_palette_get(style_name, context_name, "discrete")
year_guides <- data.table(
  date = seq(
    as.Date(format(start_date, "%Y-01-01")),
    as.Date(format(Sys.Date(), "%Y-01-01")),
    by = "1 year"
  )
)

# Optional if you have not synced locally yet:
# invisible(lapply(series_ids, investdatar::sync_local_fred_data))

get_fred_local <- function(id, label = id) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(id))
  dt[, date := as.Date(date)]
  dt[, value := as.numeric(value)]
  dt[, series := label]
  dt[!is.na(value)]
}

cut_since <- function(dt, from = start_date) {
  dt[date >= from]
}

calc_yoy <- function(dt) {
  out <- data.table::copy(dt)
  data.table::setorder(out, series, date)
  out[, yoy := (value / data.table::shift(value, 12L) - 1) * 100, by = series]
  out[!is.na(yoy)]
}

theme_panel <- function(p, legend_position = "bottom") {
  investlabr::viz_theme_apply(
    p,
    style = style_name,
    context = context_name,
    legend_position = legend_position,
    show_compiler = FALSE
  )
}

# -------------------------------------------------------------------------
# 1. Realized inflation
# -------------------------------------------------------------------------

infl <- data.table::rbindlist(list(
  get_fred_local("CPIAUCSL", "CPI"),
  get_fred_local("PCEPI", "PCE")
), use.names = TRUE)
infl_yoy <- cut_since(calc_yoy(infl))

p_infl <- ggplot(infl_yoy, aes(x = date, y = yoy, color = series)) +
  geom_vline(
    data = year_guides,
    aes(xintercept = date),
    inherit.aes = FALSE,
    color = style$grid,
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_hline(yintercept = 2, linetype = "dotted", linewidth = 0.35, color = style$muted) +
  geom_line(linewidth = 0.75, na.rm = TRUE) +
  scale_color_manual(values = c("CPI" = palette[1], "PCE" = palette[2])) +
  labs(
    title = "Realized Inflation",
    subtitle = "CPI and PCE inflation are shown as year-over-year changes; dotted line marks 2%.",
    x = NULL,
    y = "YoY inflation (%)",
    color = NULL
  )
p_infl <- theme_panel(p_infl, legend_position = "bottom")

# -------------------------------------------------------------------------
# 2. Market inflation expectations
# -------------------------------------------------------------------------

breakeven <- data.table::rbindlist(list(
  get_fred_local("T5YIE", "5Y breakeven"),
  get_fred_local("T5YIFR", "5Y5Y forward")
), use.names = TRUE)
breakeven <- cut_since(breakeven)

p_breakeven <- ggplot(breakeven, aes(x = date, y = value, color = series)) +
  geom_vline(
    data = year_guides,
    aes(xintercept = date),
    inherit.aes = FALSE,
    color = style$grid,
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_hline(yintercept = 2, linetype = "dotted", linewidth = 0.35, color = style$muted) +
  geom_line(linewidth = 0.75, na.rm = TRUE) +
  scale_color_manual(values = c(
    "5Y breakeven" = palette[3],
    "5Y5Y forward" = palette[2]
  )) +
  labs(
    title = "Market Inflation Expectations",
    subtitle = "5Y breakeven and 5Y5Y forward inflation show whether markets expect inflation to stay anchored.",
    x = NULL,
    y = "Inflation expectation (%)",
    color = NULL
  )
p_breakeven <- theme_panel(p_breakeven, legend_position = "bottom")

# -------------------------------------------------------------------------
# 3. Labor-market cooling / slack
# -------------------------------------------------------------------------

unrate <- cut_since(get_fred_local("UNRATE", "Unemployment rate"))
unrate[, measure := "Unemployment rate"]
unrate[, plot_value := value]
unrate[, unit := "%"]

payems <- get_fred_local("PAYEMS", "Payroll momentum")
data.table::setorder(payems, date)
payems[, payroll_change_k := value - data.table::shift(value)]
payems[, payroll_change_3m_avg := data.table::frollmean(payroll_change_k, n = 3L, align = "right", na.rm = TRUE)]
payems[, payroll_change_3m_avg_plot := pmax(payroll_change_3m_avg, -2500)]
payroll_momentum <- cut_since(payems[!is.na(payroll_change_3m_avg), .(
  date,
  series = "Payroll momentum",
  value = payroll_change_3m_avg,
  measure = "Payroll gains, 3m avg",
  plot_value = payroll_change_3m_avg_plot,
  unit = "thousands"
)])

labor_slack <- data.table::rbindlist(list(
  unrate[, .(date, series, value, measure, plot_value, unit)],
  payroll_momentum
), use.names = TRUE, fill = TRUE)
payroll_floor <- data.table(
  date = min(labor_slack$date, na.rm = TRUE),
  plot_value = -2500,
  measure = "Payroll gains, 3m avg"
)

p_labor <- ggplot(labor_slack, aes(x = date, y = plot_value)) +
  geom_vline(
    data = year_guides,
    aes(xintercept = date),
    inherit.aes = FALSE,
    color = style$grid,
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_hline(
    data = labor_slack[measure == "Payroll gains, 3m avg", .(measure = unique(measure), yint = 0)],
    aes(yintercept = yint),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.35,
    color = style$muted
  ) +
  geom_blank(data = payroll_floor, aes(x = date, y = plot_value)) +
  geom_line(color = style$accent, linewidth = 0.75, na.rm = TRUE) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  labs(
    title = "Labor Market Slack",
    subtitle = "Unemployment captures slack; payroll momentum shows whether job creation is cooling.",
    x = NULL,
    y = NULL
  )
p_labor <- theme_panel(p_labor, legend_position = "none")

# -------------------------------------------------------------------------
# 4. Wage pressure
# -------------------------------------------------------------------------

ahetpi <- get_fred_local("AHETPI", "Average hourly earnings")
wage_yoy <- cut_since(calc_yoy(ahetpi))

p_wage <- ggplot(wage_yoy, aes(x = date, y = yoy)) +
  geom_vline(
    data = year_guides,
    aes(xintercept = date),
    inherit.aes = FALSE,
    color = style$grid,
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_hline(yintercept = 3, linetype = "dotted", linewidth = 0.35, color = style$muted) +
  geom_line(color = style$resistance, linewidth = 0.75, na.rm = TRUE) +
  labs(
    title = "Wage Pressure",
    subtitle = "Average hourly earnings growth is shown year over year; dotted line marks 3%.",
    x = NULL,
    y = "YoY growth (%)"
  )
p_wage <- theme_panel(p_wage, legend_position = "none")

# -------------------------------------------------------------------------
# Output
# -------------------------------------------------------------------------

dashboard <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(
    p_infl, p_breakeven,
    p_labor, p_wage
  ),
  n_rows = 2,
  n_cols = 2,
  title = "Inflation and Labor Dashboard",
  bottom = paste(
    "Read clockwise: realized inflation shows what households and firms have already experienced;",
    "market inflation expectations show whether investors see inflation staying anchored;",
    "labor slack and payroll momentum show whether demand for workers is cooling;",
    "and wage growth indicates whether labor-cost pressure remains consistent with a durable disinflation path.",
    "FRED inputs: CPIAUCSL, PCEPI, T5YIE, T5YIFR, UNRATE, PAYEMS, AHETPI."
  ),
  style = style_name,
  context = context_name
)

print(dashboard)
