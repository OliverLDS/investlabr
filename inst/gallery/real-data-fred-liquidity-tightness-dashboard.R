library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

series_ids <- c(
  "IORB",
  "RRPONTSYAWARD",
  "SOFR",
  "OBFR",
  "EFFR",
  "DTB4WK"
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

roll_mean <- function(x, n = 20L) {
  data.table::frollmean(x, n = n, align = "right", na.rm = TRUE)
}

raw <- rbindlist(
  lapply(series_ids, function(id) tryCatch(get_fred_local(id), error = function(e) NULL)),
  fill = TRUE
)
raw <- raw[!is.na(date) & !is.na(value)]

wide_dt <- dcast(raw, date ~ series, value.var = "value")
setorder(wide_dt, date)

wide_dt[, `:=`(
  spread_sofr_iorb = SOFR - IORB,
  spread_obfr_iorb = OBFR - IORB,
  spread_effr_iorb = EFFR - IORB,
  spread_sofr_rrp = SOFR - RRPONTSYAWARD,
  spread_obfr_rrp = OBFR - RRPONTSYAWARD,
  spread_effr_rrp = EFFR - RRPONTSYAWARD,
  spread_tbill_rrp = DTB4WK - RRPONTSYAWARD
)]

wide_dt[, `:=`(
  spread_sofr_iorb_20 = roll_mean(spread_sofr_iorb, 20),
  spread_obfr_iorb_20 = roll_mean(spread_obfr_iorb, 20),
  spread_sofr_rrp_20 = roll_mean(spread_sofr_rrp, 20),
  spread_obfr_rrp_20 = roll_mean(spread_obfr_rrp, 20),
  spread_tbill_rrp_20 = roll_mean(spread_tbill_rrp, 20)
)]

dt_iorb <- melt(
  wide_dt[, .(date, spread_sofr_iorb_20, spread_obfr_iorb_20, spread_effr_iorb)],
  id.vars = "date",
  variable.name = "spread",
  value.name = "bps"
)
dt_iorb[, bps := 100 * bps]
dt_iorb <- dt_iorb[date >= as.Date("2021-07-29")]

p_iorb <- ggplot(dt_iorb, aes(x = date, y = bps, color = spread)) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", color = investlabr::viz_style_get("institutional_blue", "report")$muted) +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  scale_color_manual(values = investlabr::viz_palette_get("institutional_blue", "report", "discrete")[1:3]) +
  labs(
    title = "Reserve tightness proxy",
    subtitle = "Market rate minus IORB in bps",
    x = NULL,
    y = "Basis points"
  )
p_iorb <- investlabr::viz_theme_apply(
  p_iorb,
  style = "institutional_blue",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

dt_rrp <- melt(
  wide_dt[, .(date, spread_sofr_rrp_20, spread_obfr_rrp_20, spread_effr_rrp)],
  id.vars = "date",
  variable.name = "spread",
  value.name = "bps"
)
dt_rrp[, bps := 100 * bps]
dt_rrp <- dt_rrp[date >= as.Date("2013-09-23")]

p_rrp <- ggplot(dt_rrp, aes(x = date, y = bps, color = spread)) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", color = investlabr::viz_style_get("institutional_blue", "report")$muted) +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  scale_color_manual(values = investlabr::viz_palette_get("institutional_blue", "report", "discrete")[1:3]) +
  labs(
    title = "Floor-binding / cash abundance proxy",
    subtitle = "Market rate minus ON RRP in bps",
    x = NULL,
    y = "Basis points"
  )
p_rrp <- investlabr::viz_theme_apply(
  p_rrp,
  style = "institutional_blue",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

dt_bill <- wide_dt[!is.na(spread_tbill_rrp_20), .(date, bps = 100 * spread_tbill_rrp_20)]

p_bill <- ggplot(dt_bill, aes(x = date, y = bps)) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", color = investlabr::viz_style_get("institutional_blue", "report")$muted) +
  geom_line(color = investlabr::viz_style_get("institutional_blue", "report")$accent2, linewidth = 0.55, na.rm = TRUE) +
  labs(
    title = "Bills versus the floor",
    subtitle = "4W T-bill minus ON RRP in bps",
    x = NULL,
    y = "Basis points"
  )
p_bill <- investlabr::viz_theme_apply(
  p_bill,
  style = "institutional_blue",
  context = "report",
  show_compiler = FALSE
)

dt_state <- copy(wide_dt)[
  ,
  .(
    date,
    reserve_tightness = spread_sofr_iorb_20,
    floor_binding = -abs(spread_sofr_rrp_20)
  )
]
dt_state <- dt_state[!is.na(reserve_tightness) & !is.na(floor_binding)]

p_state <- ggplot(dt_state, aes(x = reserve_tightness * 100, y = floor_binding * 100)) +
  geom_point(color = investlabr::viz_style_get("institutional_blue", "report")$accent, alpha = 0.35, size = 1.2) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", color = investlabr::viz_style_get("institutional_blue", "report")$muted) +
  geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed", color = investlabr::viz_style_get("institutional_blue", "report")$muted) +
  labs(
    title = "Liquidity regime map",
    subtitle = "SOFR-IORB versus negative distance to ON RRP",
    x = "Reserve tightness proxy (bps)",
    y = "Closer to floor (bps)"
  )
p_state <- investlabr::viz_theme_apply(
  p_state,
  style = "institutional_blue",
  context = "report",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(
    p_iorb, p_rrp,
    p_bill, p_state
  ),
  n_rows = 2,
  n_cols = 2,
  title = "Liquidity Tightness and Floor-Binding Dashboard",
  bottom = paste(
    "Reading guide:",
    "Market - IORB above zero suggests tighter reserve conditions.",
    "Market - ON RRP near zero suggests a floor-dominated abundant-cash regime.",
    sep = " "
  ),
  style = "institutional_blue",
  context = "report"
)
