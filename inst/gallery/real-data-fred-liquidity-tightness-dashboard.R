library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

# Short-term liquidity-tightness dashboard.
# Data access remains in investdatar; this script focuses on short-horizon
# transformation, interpretation, and briefing-ready visualization.

lookback_days <- 92L
end_date <- Sys.Date()
start_date <- end_date - lookback_days
style_name <- "institutional_blue"
context_name <- "report"
style <- investlabr::viz_style_get(style_name, context_name)
palette <- investlabr::viz_palette_get(style_name, context_name, "discrete")

series_ids <- c(
  "IORB",        # Interest on reserve balances
  "EFFR",        # Effective federal funds rate
  "SOFR",        # Secured overnight financing rate
  "OBFR",        # Overnight bank funding rate
  "DTB4WK",      # 4-week Treasury bill secondary-market rate
  "WRBWFRBL",    # Reserve balances with Federal Reserve Banks
  "WSHOTSL",     # Securities held outright
  "WCICL",       # Currency in circulation
  "WORAL",       # Assets: repurchase agreements
  "WLRRAL",      # Liabilities: reverse repurchase agreements
  "WDTGAL"       # Treasury General Account
)

# Optional if local FRED cache is not up to date:
# invisible(lapply(series_ids, investdatar::sync_local_fred_data))

# -------------------------------------------------------------------------
# Data prep helpers
# -------------------------------------------------------------------------

get_fred_local <- function(id, label = id) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(id))
  dt[, date := as.Date(date)]
  dt[, value := as.numeric(value)]
  dt[, series := label]
  dt[!is.na(value)]
}

zscore_window <- function(x) {
  sdv <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / sdv
}

make_hguides <- function(x, gap) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng))) return(numeric())
  seq(floor(rng[1] / gap) * gap, ceiling(rng[2] / gap) * gap, by = gap)
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

to_level_change_trn <- function(x) {
  base <- x[which(is.finite(x))[1L]]
  (x - base) / 1e6
}

# -------------------------------------------------------------------------
# Load FRED series and create a wide short-window table
# -------------------------------------------------------------------------

raw <- data.table::rbindlist(
  lapply(series_ids, function(id) tryCatch(get_fred_local(id), error = function(e) NULL)),
  fill = TRUE
)
raw <- raw[!is.na(date) & !is.na(value)]

wide_all <- data.table::dcast(raw, date ~ series, value.var = "value")
data.table::setorder(wide_all, date)
value_cols <- setdiff(names(wide_all), "date")
wide_all[, (value_cols) := lapply(.SD, data.table::nafill, type = "locf"), .SDcols = value_cols]

wide_dt <- wide_all[date >= start_date & date <= end_date]
if (nrow(wide_dt) == 0L) {
  stop("No FRED observations available in the requested short-term window.", call. = FALSE)
}

daily_guides <- data.table(date = seq(min(wide_dt$date), max(wide_dt$date), by = "day"))
month_breaks <- data.table(date = seq(
  as.Date(format(min(wide_dt$date), "%Y-%m-01")),
  as.Date(format(max(wide_dt$date), "%Y-%m-01")),
  by = "month"
))
date_breaks <- sort(unique(c(
  month_breaks[date >= min(wide_dt$date) & date <= max(wide_dt$date), date],
  daily_guides[as.integer(format(date, "%d")) %in% c(5L, 10L, 15L, 20L, 25L), date]
)))
label_short_dates <- function(x) {
  day <- as.integer(format(x, "%d"))
  ifelse(day == 1L, format(x, "%b"), as.character(day))
}

wide_dt[, `:=`(
  effr_iorb_bps = 100 * (EFFR - IORB),
  sofr_iorb_bps = 100 * (SOFR - IORB),
  obfr_iorb_bps = 100 * (OBFR - IORB),
  sofr_effr_bps = 100 * (SOFR - EFFR),
  bill_iorb_bps = 100 * (DTB4WK - IORB),
  reserves_trn = WRBWFRBL / 1e6,
  reserves_change_trn = to_level_change_trn(WRBWFRBL),
  securities_less_currency_change_trn = to_level_change_trn(WSHOTSL - WCICL),
  repo_less_reverse_repo_change_trn = to_level_change_trn(WORAL - WLRRAL),
  tga_change_trn = to_level_change_trn(WDTGAL)
)]

wide_dt[, funding_pressure_bps := rowMeans(
  .SD,
  na.rm = TRUE
), .SDcols = c("effr_iorb_bps", "sofr_iorb_bps", "obfr_iorb_bps")]

wide_dt[, `:=`(
  reserve_drain_score = zscore_window(-reserves_change_trn),
  funding_pressure_score = zscore_window(funding_pressure_bps),
  collateral_pressure_score = zscore_window(bill_iorb_bps)
)]
wide_dt[, integrated_tightness_score := rowMeans(
  .SD,
  na.rm = TRUE
), .SDcols = c("reserve_drain_score", "funding_pressure_score", "collateral_pressure_score")]

# -------------------------------------------------------------------------
# Panel 1: Are funding rates pressing above the administered floor?
# -------------------------------------------------------------------------

funding_dt <- data.table::melt(
  wide_dt[, .(date, effr_iorb_bps, sofr_iorb_bps, obfr_iorb_bps)],
  id.vars = "date",
  variable.name = "indicator",
  value.name = "bps"
)
funding_dt[, indicator := factor(
  indicator,
  levels = c("effr_iorb_bps", "sofr_iorb_bps", "obfr_iorb_bps"),
  labels = c("EFFR - IORB", "SOFR - IORB", "OBFR - IORB")
)]
funding_guides <- data.table(y = make_hguides(funding_dt$bps, 0.1))

p_funding <- ggplot(funding_dt, aes(date, bps, color = indicator)) +
  geom_vline(data = daily_guides, aes(xintercept = date), inherit.aes = FALSE, color = style$grid, linewidth = 0.18, alpha = 0.35) +
  geom_hline(data = funding_guides, aes(yintercept = y), inherit.aes = FALSE, color = style$grid, linewidth = 0.2, alpha = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", color = style$muted) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  scale_color_manual(values = c(
    "EFFR - IORB" = "#0B3D91",
    "SOFR - IORB" = "#D1495B",
    "OBFR - IORB" = "#2E8B57"
  )) +
  scale_x_date(breaks = date_breaks, labels = label_short_dates) +
  labs(
    title = "1. Are funding rates pressing above the floor?",
    subtitle = "Positive spreads versus IORB indicate money-market rates are trading rich to the administered floor.",
    x = NULL,
    y = "Basis points",
    color = NULL
  )
p_funding <- theme_panel(p_funding)

# -------------------------------------------------------------------------
# Panel 2: Is reserve / bank liquidity getting tighter?
# -------------------------------------------------------------------------

reserve_dt <- data.table::melt(
  wide_dt[, .(
    date,
    reserves_change_trn,
    securities_less_currency_change_trn,
    repo_less_reverse_repo_change_trn,
    tga_change_trn
  )],
  id.vars = "date",
  variable.name = "indicator",
  value.name = "change_trn"
)
reserve_dt[, indicator := factor(
  indicator,
  levels = c(
    "reserves_change_trn",
    "securities_less_currency_change_trn",
    "repo_less_reverse_repo_change_trn",
    "tga_change_trn"
  ),
  labels = c(
    "Reserve balances",
    "Securities - currency",
    "Repos - reverse repos",
    "Treasury General Account"
  )
)]
reserve_guides <- data.table(y = make_hguides(reserve_dt$change_trn, 0.01))

p_reserves <- ggplot(reserve_dt, aes(date, change_trn, color = indicator)) +
  geom_vline(data = daily_guides, aes(xintercept = date), inherit.aes = FALSE, color = style$grid, linewidth = 0.18, alpha = 0.35) +
  geom_hline(data = reserve_guides, aes(yintercept = y), inherit.aes = FALSE, color = style$grid, linewidth = 0.18, alpha = 0.45) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", color = style$muted) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_color_manual(values = c(
    "Reserve balances" = "#0B3D91",
    "Securities - currency" = "#D1495B",
    "Repos - reverse repos" = "#2E8B57",
    "Treasury General Account" = "#7B2CBF"
  )) +
  scale_x_date(breaks = date_breaks, labels = label_short_dates) +
  labs(
    title = "2. Is bank-system liquidity being drained?",
    subtitle = "Balance-sheet plumbing lines are shown as changes since the start of the 3-month window.",
    x = NULL,
    y = "Change since window start (USD tn)",
    color = NULL
  )
p_reserves <- theme_panel(p_reserves)

# -------------------------------------------------------------------------
# Panel 3: Is collateral / cash competition showing up elsewhere?
# -------------------------------------------------------------------------

transmission_dt <- data.table::melt(
  wide_dt[, .(date, bill_iorb_bps, sofr_effr_bps)],
  id.vars = "date",
  variable.name = "indicator",
  value.name = "bps"
)
transmission_dt[, indicator := factor(
  indicator,
  levels = c("bill_iorb_bps", "sofr_effr_bps"),
  labels = c("4W bill - IORB", "SOFR - EFFR")
)]
transmission_guides <- data.table(y = make_hguides(transmission_dt$bps, 0.1))

p_transmission <- ggplot(transmission_dt, aes(date, bps, color = indicator)) +
  geom_vline(data = daily_guides, aes(xintercept = date), inherit.aes = FALSE, color = style$grid, linewidth = 0.18, alpha = 0.35) +
  geom_hline(data = transmission_guides, aes(yintercept = y), inherit.aes = FALSE, color = style$grid, linewidth = 0.2, alpha = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", color = style$muted) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  scale_color_manual(values = c(
    "4W bill - IORB" = "#E76F51",
    "SOFR - EFFR" = "#264653"
  )) +
  scale_x_date(breaks = date_breaks, labels = label_short_dates) +
  labs(
    title = "3. Is tightness transmitting across short-end instruments?",
    subtitle = "Bills versus IORB and SOFR versus EFFR add a collateral/cash-competition lens beyond reserves.",
    x = NULL,
    y = "Basis points",
    color = NULL
  )
p_transmission <- theme_panel(p_transmission)

# -------------------------------------------------------------------------
# Panel 4: What is the integrated liquidity message right now?
# -------------------------------------------------------------------------

score_dt <- wide_dt[is.finite(integrated_tightness_score), .(
  date,
  integrated_tightness_score,
  regime = data.table::fcase(
    integrated_tightness_score >= 0.75, "Tightening",
    integrated_tightness_score <= -0.75, "Easing",
    default = "Neutral"
  )
)]

latest_score <- score_dt[.N]
latest_label <- paste0(
  "Latest: ", latest_score$regime,
  " (", sprintf("%.2f", latest_score$integrated_tightness_score), ")"
)

p_score <- ggplot(score_dt, aes(date, integrated_tightness_score)) +
  geom_vline(data = daily_guides, aes(xintercept = date), inherit.aes = FALSE, color = style$grid, linewidth = 0.18, alpha = 0.35) +
  geom_hline(yintercept = c(-0.75, 0.75), linewidth = 0.3, linetype = "dotted", color = style$muted) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dashed", color = style$muted) +
  geom_col(aes(fill = regime), width = 1.0, alpha = 0.85, na.rm = TRUE) +
  scale_fill_manual(values = c(
    "Easing" = "#4F8A8B",
    "Neutral" = "#C9B37E",
    "Tightening" = "#B24C45"
  )) +
  scale_x_date(breaks = date_breaks, labels = label_short_dates) +
  labs(
    title = "4. Integrated short-term liquidity message",
    subtitle = latest_label,
    x = NULL,
    y = "Normalized tightness score",
    fill = NULL
  )
p_score <- theme_panel(p_score)

# -------------------------------------------------------------------------
# Output
# -------------------------------------------------------------------------

dashboard <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(
    p_funding, p_reserves,
    p_transmission, p_score
  ),
  n_rows = 2,
  n_cols = 2,
  title = "Short-Term Liquidity Tightness Dashboard",
  bottom = paste(
    "Window:",
    paste0(format(start_date, "%Y-%m-%d"), " to ", format(end_date, "%Y-%m-%d"), "."),
    "Read clockwise: funding-rate pressure, balance-sheet plumbing, cross-market transmission, and a normalized synthesis score.",
    "The synthesis score averages three displayed-window z-scores: average EFFR/SOFR/OBFR pressure versus IORB, reserve drain measured as the negative change in reserve balances, and 4W bill pressure versus IORB.",
    "FRED inputs: IORB, EFFR, SOFR, OBFR, DTB4WK, WRBWFRBL, WSHOTSL, WCICL, WORAL, WLRRAL, WDTGAL."
  ),
  style = style_name,
  context = context_name
)

print(dashboard)
