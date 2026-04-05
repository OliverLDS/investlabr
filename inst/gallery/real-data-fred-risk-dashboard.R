library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

series_map <- c(
  VIXCLS = "VIX (VIXCLS)",
  USEPUINDXD = "US EPU (USEPUINDXD)",
  WLEMUINDXD = "US EMU (WLEMUINDXD)"
)

# Optional if you have not synced locally yet:
# invisible(lapply(names(series_map), investdatar::sync_local_fred_data))

get_fred_local <- function(id, label = id) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(id))
  dt[, date := as.Date(date)]
  dt[, value := as.numeric(value)]
  dt[, series := label]
  dt[!is.na(value)]
}

roll_cor <- function(x, y, k = 12L) {
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i >= k) {
      xi <- x[(i - k + 1L):i]
      yi <- y[(i - k + 1L):i]
      if (all(is.na(xi)) || all(is.na(yi))) next
      out[i] <- suppressWarnings(stats::cor(xi, yi, use = "pairwise.complete.obs"))
    }
  }
  out
}

dt_all <- data.table::rbindlist(
  lapply(names(series_map), function(id) get_fred_local(id, series_map[[id]])),
  use.names = TRUE,
  fill = TRUE
)

dt_monthly <- dt_all[
  , .(value = mean(value, na.rm = TRUE)),
  by = .(series, date = as.Date(format(date, "%Y-%m-01")))
]

dt_wide <- dcast(dt_monthly, date ~ series, value.var = "value")
setorder(dt_wide, date)

dt_monthly_z <- copy(dt_monthly)
dt_monthly_z[
  , z := {
    x <- as.numeric(value)
    (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  },
  by = series
]

col_vix <- "VIX (VIXCLS)"
col_epu <- "US EPU (USEPUINDXD)"
col_emu <- "US EMU (WLEMUINDXD)"

dt_cor <- dt_wide[, .(date)]
dt_cor[, `corr(VIX,EPU)` := roll_cor(dt_wide[[col_vix]], dt_wide[[col_epu]], k = 12L)]
dt_cor[, `corr(VIX,EMU)` := roll_cor(dt_wide[[col_vix]], dt_wide[[col_emu]], k = 12L)]
dt_cor[, `corr(EPU,EMU)` := roll_cor(dt_wide[[col_epu]], dt_wide[[col_emu]], k = 12L)]

dt_cor_long <- melt(
  dt_cor,
  id.vars = "date",
  variable.name = "pair",
  value.name = "corr"
)

p_levels <- ggplot(dt_monthly, aes(x = date, y = value)) +
  geom_line(color = investlabr::viz_style_get("policy_memo", "report")$accent, linewidth = 0.8) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Raw levels (monthly average)",
    subtitle = "Each series keeps its own scale",
    x = NULL,
    y = NULL
  )
p_levels <- investlabr::viz_theme_apply(
  p_levels,
  style = "policy_memo",
  context = "report",
  show_compiler = FALSE
)

p_z <- ggplot(dt_monthly_z, aes(x = date, y = z, color = series)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = investlabr::viz_palette_get("policy_memo", "report", "discrete")[1:3]) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Standardized (z-score)",
    subtitle = "Comparable scale highlights timing misalignment",
    x = NULL,
    y = "z-score"
  )
p_z <- investlabr::viz_theme_apply(
  p_z,
  style = "policy_memo",
  context = "report",
  legend_position = "none",
  show_compiler = FALSE
)

p_cor <- ggplot(dt_cor_long, aes(x = date, y = corr)) +
  geom_line(color = investlabr::viz_style_get("policy_memo", "report")$accent2, linewidth = 0.8) +
  facet_wrap(~ pair, ncol = 1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Rolling 12-month correlations",
    subtitle = "Alignment stability across the three risk proxies",
    x = NULL,
    y = "correlation"
  )
p_cor <- investlabr::viz_theme_apply(
  p_cor,
  style = "policy_memo",
  context = "report",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_levels, p_z, p_cor),
  n_rows = 3,
  n_cols = 1,
  title = "Risk indicators are messy and not aligned",
  bottom = "VIX (market-implied volatility) versus EPU and EMU (news-based uncertainty) often diverge in timing and persistence.",
  style = "policy_memo",
  context = "report"
)

# ---- Daily comparison: VIX vs EPU only ----

series_map_daily <- c(
  VIXCLS = "VIX",
  USEPUINDXD = "EPU"
)

dt_daily_all <- data.table::rbindlist(
  lapply(names(series_map_daily), function(id) get_fred_local(id, series_map_daily[[id]])),
  use.names = TRUE,
  fill = TRUE
)

dt_daily_wide <- dcast(dt_daily_all, date ~ series, value.var = "value")
setorder(dt_daily_wide, date)
dt_daily_wide <- dt_daily_wide[!is.na(VIX) & !is.na(EPU)]

dt_daily_long <- melt(
  dt_daily_wide,
  id.vars = "date",
  variable.name = "series",
  value.name = "value"
)
dt_daily_long[
  , z := {
    x <- as.numeric(value)
    (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  },
  by = series
]

dt_daily_wide[, corr_60d := roll_cor(VIX, EPU, k = 60L)]

p_daily_levels <- ggplot(dt_daily_long, aes(x = date, y = value)) +
  geom_line(color = investlabr::viz_style_get("institutional_blue", "report")$accent, linewidth = 0.8) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Daily levels (aligned dates only)",
    x = NULL,
    y = NULL
  )
p_daily_levels <- investlabr::viz_theme_apply(
  p_daily_levels,
  style = "institutional_blue",
  context = "report",
  show_compiler = FALSE
)

p_daily_z <- ggplot(dt_daily_long, aes(x = date, y = z, color = series)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = investlabr::viz_palette_get("institutional_blue", "report", "discrete")[1:2]) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Daily standardized (z-score)",
    subtitle = "VIX versus EPU misalignment on aligned dates",
    x = NULL,
    y = "z-score"
  )
p_daily_z <- investlabr::viz_theme_apply(
  p_daily_z,
  style = "institutional_blue",
  context = "report",
  legend_position = "none",
  show_compiler = FALSE
)

p_daily_cor <- ggplot(dt_daily_wide, aes(x = date, y = corr_60d)) +
  geom_line(color = investlabr::viz_style_get("institutional_blue", "report")$accent2, linewidth = 0.8) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Rolling correlation (60 trading days)",
    x = NULL,
    y = "correlation"
  )
p_daily_cor <- investlabr::viz_theme_apply(
  p_daily_cor,
  style = "institutional_blue",
  context = "report",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_daily_levels, p_daily_z, p_daily_cor),
  n_rows = 3,
  n_cols = 1,
  title = "Daily comparison: VIX versus EPU",
  bottom = "Same dates, different mechanisms: market-implied volatility and news-based policy uncertainty often diverge.",
  style = "institutional_blue",
  context = "report"
)
