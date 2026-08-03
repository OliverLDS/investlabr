library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

series_ids <- c("T10Y2Y", "T10Y3M", "DFII5", "DFII7", "DFII10", "DFII20", "DFII30", "DLTIIT")

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

t10y2y <- get_fred_local("T10Y2Y", "10Y-2Y")
t10y3m <- get_fred_local("T10Y3M", "10Y-3M")
curve_spreads <- rbindlist(list(t10y2y, t10y3m), use.names = TRUE)
curve_spreads <- cut_since(curve_spreads)
year_guides <- data.table(
  date = seq(
    as.Date(format(min(curve_spreads$date, na.rm = TRUE), "%Y-01-01")),
    as.Date(format(max(curve_spreads$date, na.rm = TRUE), "%Y-01-01")),
    by = "1 year"
  )
)
curve_colors <- c(
  "10Y-2Y" = "#12355B",
  "10Y-3M" = "#D1495B"
)

p_curve_spread <- ggplot(curve_spreads, aes(x = date, y = value, color = series)) +
  geom_vline(
    data = year_guides,
    aes(xintercept = date),
    inherit.aes = FALSE,
    color = investlabr::viz_style_get("institutional_blue", "report")$grid,
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = investlabr::viz_style_get("institutional_blue", "report")$muted) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = curve_colors) +
  labs(
    title = "Yield Curve Spreads",
    subtitle = "10Y-2Y and 10Y-3M",
    x = NULL,
    y = "Spread (percentage points)"
  )
p_curve_spread <- investlabr::viz_theme_apply(
  p_curve_spread,
  style = "institutional_blue",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

real_yields <- rbindlist(list(
  get_fred_local("DFII10", "10Y TIPS"),
  get_fred_local("DLTIIT", "Long-term avg real")
), use.names = TRUE)
real_yields <- cut_since(real_yields)
real_colors <- c(
  "10Y TIPS" = "#0B6E69",
  "Long-term avg real" = "#C16622"
)

p_real <- ggplot(real_yields, aes(x = date, y = value, color = series)) +
  geom_vline(
    data = year_guides,
    aes(xintercept = date),
    inherit.aes = FALSE,
    color = investlabr::viz_style_get("institutional_blue", "report")$grid,
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = real_colors) +
  labs(
    title = "Real Treasury Yield Anchor",
    subtitle = "10Y TIPS versus the long-term average real rate",
    x = NULL,
    y = "Real yield (%)"
  )
p_real <- investlabr::viz_theme_apply(
  p_real,
  style = "institutional_blue",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_curve_spread, p_real),
  n_rows = 1,
  n_cols = 2,
  title = "Curve and Real-Yield Dashboard",
  bottom = "FRED inputs: T10Y2Y, T10Y3M, DFII10, DLTIIT.",
  style = "institutional_blue",
  context = "report"
)
