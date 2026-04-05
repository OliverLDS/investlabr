library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

start_date <- as.Date("2015-01-01")
RESERVES_ID <- "WRESBAL"

# Optional if you have not synced locally yet:
# invisible(lapply(
#   c("EFFR", "IORB", "SOFR", "WTREGEN", RESERVES_ID, "TREAST", "WREPO", "RRPONTSYD", "WALCL"),
#   investdatar::sync_local_fred_data
# ))

get_fred_series <- function(series_id, label = series_id) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(series_id))
  dt <- dt[date >= start_date & !is.na(value), .(date, value = as.numeric(value))]
  dt[, series := label]
  setorder(dt, date)
  dt
}

to_index100 <- function(dt, label = unique(dt$series)) {
  out <- copy(dt)[order(date)]
  base <- out[!is.na(value)][1, value]
  out[, value := 100 * value / base]
  out[, series := label]
  out
}

make_spread <- function(series_a, series_b, label) {
  a <- copy(series_a)[, .(date, a = value)]
  b <- copy(series_b)[, .(date, b = value)]
  out <- merge(a, b, by = "date", all = FALSE)
  out[, `:=`(value = a - b, series = label)]
  out[, .(date, value, series)]
}

add_panel <- function(dt, panel_name) {
  out <- copy(dt)
  out[, panel := panel_name]
  out
}

theme_panel <- function(p, show_compiler = FALSE) {
  investlabr::viz_theme_apply(
    p,
    style = "policy_memo",
    context = "report",
    legend_position = "bottom",
    show_compiler = show_compiler
  )
}

dt_effr <- get_fred_series("EFFR", "EFFR")
dt_iorb <- get_fred_series("IORB", "IORB")
dt_sofr <- get_fred_series("SOFR", "SOFR")
dt_tga <- get_fred_series("WTREGEN", "Treasury General Account")
dt_reserves <- get_fred_series(RESERVES_ID, "Reserve balances")
dt_treast <- get_fred_series("TREAST", "Treasuries held")
dt_repo <- get_fred_series("WREPO", "Repo assets")
dt_rrp <- get_fred_series("RRPONTSYD", "ON RRP usage")
dt_walcl <- get_fred_series("WALCL", "Fed total assets")

p1_dt <- rbindlist(list(
  make_spread(dt_effr, dt_iorb, "EFFR - IORB"),
  make_spread(dt_sofr, dt_iorb, "SOFR - IORB")
), fill = TRUE)

p1 <- ggplot(p1_dt, aes(date, value, color = series)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  labs(
    title = "Implementation spreads to IORB",
    subtitle = "How closely overnight rates trade around the administered floor",
    x = NULL,
    y = "Spread (percentage points)",
    color = NULL
  )
p1 <- theme_panel(p1, show_compiler = FALSE)

p2_dt <- rbindlist(list(
  to_index100(dt_tga, "TGA (idx)"),
  to_index100(dt_reserves, "Reserves (idx)"),
  to_index100(dt_treast, "Treasuries held (idx)")
), fill = TRUE)

p2 <- ggplot(p2_dt, aes(date, value, color = series)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  labs(
    title = "TGA swings and reserve mechanics",
    subtitle = "Treasury cash balances, reserves, and SOMA Treasury holdings rebased to 100",
    x = NULL,
    y = "Index (start = 100)",
    color = NULL
  )
p2 <- theme_panel(p2, show_compiler = FALSE)

p3_dt <- rbindlist(list(
  add_panel(to_index100(dt_repo, "Repo assets (idx)"), "Balance sheet operations"),
  add_panel(to_index100(dt_rrp, "ON RRP usage (idx)"), "Balance sheet operations"),
  add_panel(make_spread(dt_sofr, dt_iorb, "SOFR - IORB"), "Pricing symptom")
), fill = TRUE)

p3 <- ggplot(p3_dt, aes(date, value, color = series)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  labs(
    title = "Operations footprint and pricing symptom",
    subtitle = "Balance sheet usage sits alongside a simple floor-spread signal",
    x = NULL,
    y = NULL,
    color = NULL
  )
p3 <- theme_panel(p3, show_compiler = FALSE)

p4_dt <- rbindlist(list(
  to_index100(dt_reserves, "Reserves (idx)"),
  to_index100(dt_treast, "Treasuries held (idx)"),
  to_index100(dt_walcl, "Fed total assets (idx)")
), fill = TRUE)

p4 <- ggplot(p4_dt, aes(date, value, color = series)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  labs(
    title = "Reserves and purchases footprint",
    subtitle = "A compact balance-sheet board rebased to 100",
    x = NULL,
    y = "Index (start = 100)",
    color = NULL
  )
p4 <- theme_panel(p4, show_compiler = FALSE)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p1, p2, p3, p4),
  n_rows = 2,
  n_cols = 2,
  title = "FOMC plumbing board",
  bottom = paste(
    "This board focuses on the implementation-plumbing angle that is less visible in standard policy dashboards:",
    "overnight-rate spreads to IORB, Treasury cash swings, reserve mechanics, repo and ON RRP usage, and the broader balance-sheet footprint."
  ),
  style = "policy_memo",
  context = "report",
  show_compiler = TRUE
)
