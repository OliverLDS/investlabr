library(data.table)
library(ggplot2)
library(scales)
library(investdatar)
library(investlabr)

RECENT_ED <- Sys.Date()
RECENT_BG <- seq(RECENT_ED, by = "-6 months", length.out = 2L)[2L]
START_DATE <- RECENT_BG

# Optional if you have not synced locally yet:
# invisible(lapply(
#   c("WALCL", "WORAL", "WSHOTSL", "WSHOMCB", "WCICL", "WLRRAL", "WDTGAL", "WOLCL", "WRBWFRBL"),
#   investdatar::sync_local_fred_data
# ))

get_series <- function(series_id, label) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(series_id))
  dt <- dt[date >= START_DATE & !is.na(value), .(date, value = as.numeric(value))]
  setorder(dt, date)
  dt[, `:=`(
    series = label,
    value_trn = value / 1e6
  )]
  dt
}

to_recent_delta <- function(dt, recent_bg, label = unique(dt$series)) {
  out <- copy(dt)[date >= recent_bg & date <= RECENT_ED]
  base <- out[1, value_trn]
  out[, `:=`(
    value = value_trn - base,
    series = label
  )]
  out[, .(date, value, series)]
}

assets_dt <- rbindlist(list(
  get_series("WALCL", "Total assets"),
  get_series("WSHOTSL", "Treasuries"),
  get_series("WSHOMCB", "MBS"),
  get_series("WORAL", "Repos")
), fill = TRUE)

liab_dt <- rbindlist(list(
  get_series("WCICL", "Currency in circulation"),
  get_series("WLRRAL", "ON RRP / RRPs"),
  get_series("WDTGAL", "Treasury General Account"),
  get_series("WOLCL", "Other liabilities & capital"),
  get_series("WRBWFRBL", "Reserve balances")
), fill = TRUE)

assets_recent <- assets_dt[date >= RECENT_BG & date <= RECENT_ED]
liab_recent <- liab_dt[date >= RECENT_BG & date <= RECENT_ED]

assets_delta <- rbindlist(lapply(split(assets_dt, by = "series"), to_recent_delta, recent_bg = RECENT_BG), fill = TRUE)
liab_delta <- rbindlist(lapply(split(liab_dt, by = "series"), to_recent_delta, recent_bg = RECENT_BG), fill = TRUE)

p1 <- ggplot(assets_recent, aes(date, value_trn)) +
  geom_line(color = investlabr::viz_style_get("policy_memo", "report")$accent, linewidth = 0.85, na.rm = TRUE) +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Assets: component stock levels",
    subtitle = paste0(format(RECENT_BG, "%Y-%m-%d"), " to ", format(RECENT_ED, "%Y-%m-%d")),
    x = NULL,
    y = "USD trillions",
    color = NULL
  )
p1 <- investlabr::viz_theme_apply(
  p1,
  style = "policy_memo",
  context = "report",
  legend_position = "none",
  show_compiler = FALSE
)

p2 <- ggplot(liab_recent, aes(date, value_trn)) +
  geom_line(color = investlabr::viz_style_get("policy_memo", "report")$accent2, linewidth = 0.85, na.rm = TRUE) +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Liabilities and absorption factors: component stock levels",
    subtitle = paste0(format(RECENT_BG, "%Y-%m-%d"), " to ", format(RECENT_ED, "%Y-%m-%d")),
    x = NULL,
    y = "USD trillions",
    color = NULL
  )
p2 <- investlabr::viz_theme_apply(
  p2,
  style = "policy_memo",
  context = "report",
  legend_position = "none",
  show_compiler = FALSE
)

p3 <- ggplot(assets_delta, aes(date, value, color = series)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Assets: cumulative change since the recent anchor date",
    subtitle = "Rebased to zero at the first observation in the recent window",
    x = NULL,
    y = "Change in USD trillions",
    color = NULL
  )
p3 <- investlabr::viz_theme_apply(
  p3,
  style = "policy_memo",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

p4 <- ggplot(liab_delta, aes(date, value, color = series)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Liabilities and absorption factors: cumulative change since the recent anchor date",
    subtitle = "This makes shifts in reserves, TGA, RRP, and currency easier to compare directly",
    x = NULL,
    y = "Change in USD trillions",
    color = NULL
  )
p4 <- investlabr::viz_theme_apply(
  p4,
  style = "policy_memo",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p1, p2, p3, p4),
  n_rows = 2,
  n_cols = 2,
  title = "Federal Reserve balance-sheet mirror board",
  bottom = paste(
    "This board focuses on a distinct balance-sheet angle rather than policy-rate or corridor mechanics:",
    "asset and liability stocks are shown directly in trillions, then rebased to zero over the recent window to highlight which balance-sheet lines actually drove the latest moves."
  ),
  style = "policy_memo",
  context = "report",
  show_compiler = TRUE
)
