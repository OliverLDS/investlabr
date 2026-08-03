library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

start_date <- as.Date("1990-01-01")
reclass_date <- as.Date("2025-01-01")

series_meta <- data.table::data.table(
  id = c(
    "TOTCI",
    "DRTSCILM",
    "DRTSCIS",
    "DRSDCILM",
    "DRSDCIS",
    "DRISCFLM",
    "DRISCFS",
    "SUBLPDCILTRNQ",
    "SUBLPDCILTLNQ",
    "SUBLPDCISTCNQ"
  ),
  label = c(
    "TOTCI",
    "Std_LM",
    "Std_Small",
    "Demand_LM",
    "Demand_Small",
    "Spread_LM",
    "Spread_Small",
    "RiskPrem_LM",
    "Covenant_LM",
    "CostLine_Small"
  )
)

# Optional sync if your local FRED cache is not up to date.
# invisible(lapply(series_meta$id, investdatar::sync_local_fred_data))

get_fred_local <- function(series_id, label) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(series_id))
  if (is.null(dt) || nrow(dt) == 0L) {
    stop("Local FRED data not found for series: ", series_id)
  }
  dt[, date := as.Date(date)]
  dt[, value := as.numeric(value)]
  dt <- dt[!is.na(date) & !is.na(value) & date >= start_date]
  if (nrow(dt) == 0L) {
    stop("Local FRED data has no valid observations after start_date for series: ", series_id)
  }
  dt[, series_id := series_id]
  dt[, series := label]
  dt[]
}

quarter_start <- function(x) {
  x <- as.Date(x)
  year <- as.integer(format(x, "%Y"))
  month <- as.integer(format(x, "%m"))
  quarter_month <- ((month - 1L) %/% 3L) * 3L + 1L
  as.Date(sprintf("%04d-%02d-01", year, quarter_month))
}

yoy_pct <- function(x, lag_n) {
  out <- rep(NA_real_, length(x))
  if (length(x) <= lag_n) {
    return(out)
  }
  prev <- data.table::shift(x, n = lag_n, type = "lag")
  ok <- is.finite(x) & is.finite(prev) & prev != 0
  out[ok] <- 100 * (x[ok] / prev[ok] - 1)
  out
}

safe_row_mean <- function(dt, cols) {
  present <- intersect(cols, names(dt))
  if (!length(present)) {
    return(rep(NA_real_, nrow(dt)))
  }
  out <- rowMeans(as.matrix(dt[, ..present]), na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

series_list <- setNames(
  lapply(seq_len(nrow(series_meta)), function(i) {
    get_fred_local(series_meta$id[i], series_meta$label[i])
  }),
  series_meta$label
)

totci_q <- series_list$TOTCI[
  ,
  .(
    TOTCI_qavg = mean(value, na.rm = TRUE),
    TOTCI_qend = tail(value[is.finite(value)], 1L)
  ),
  by = .(qdate = quarter_start(date))
]
setorder(totci_q, qdate)
totci_q[, TOTCI_yoy := yoy_pct(TOTCI_qavg, lag_n = 4L)]

sloos_q <- data.table::rbindlist(
  lapply(setdiff(names(series_list), "TOTCI"), function(series_name) {
    dt <- data.table::copy(series_list[[series_name]])
    dt[, qdate := quarter_start(date)]
    dt <- dt[!duplicated(qdate, fromLast = TRUE), .(qdate, series = series_name, value)]
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)

sloos_wide <- data.table::dcast(
  sloos_q,
  qdate ~ series,
  value.var = "value"
)

monitor_q <- merge(totci_q, sloos_wide, by = "qdate", all = TRUE)
setorder(monitor_q, qdate)
monitor_q[, Standards_Avg := safe_row_mean(.SD, c("Std_LM", "Std_Small"))]
monitor_q[, Demand_Avg := safe_row_mean(.SD, c("Demand_LM", "Demand_Small"))]
monitor_q[, Terms_LM_Avg := safe_row_mean(.SD, c("Spread_LM", "RiskPrem_LM", "Covenant_LM"))]
monitor_q[, Terms_Small_Avg := safe_row_mean(.SD, c("Spread_Small", "CostLine_Small"))]
monitor_q[, Terms_All_Avg := safe_row_mean(.SD, c("Terms_LM_Avg", "Terms_Small_Avg"))]

monitor_w <- data.table::copy(series_list$TOTCI)
setorder(monitor_w, date)
monitor_w[, TOTCI_yoy := yoy_pct(value, lag_n = 52L)]

style <- "policy_memo"
context <- "report"
palette <- investlabr::viz_palette_get(style, context, "discrete")
style_resolved <- investlabr::viz_style_get(style, context)

plot_reclass_line <- function(p) {
  p +
    ggplot2::geom_vline(
      xintercept = reclass_date,
      linetype = "dashed",
      linewidth = 0.45,
      color = style_resolved$muted
    )
}

add_year_guides <- function(p, dt, date_col) {
  x <- as.Date(dt[[date_col]])
  years <- seq(
    as.Date(sprintf("%s-01-01", min(format(x, "%Y"), na.rm = TRUE))),
    as.Date(sprintf("%s-01-01", max(format(x, "%Y"), na.rm = TRUE))),
    by = "1 year"
  )
  p +
    ggplot2::geom_vline(
      xintercept = years,
      linewidth = 0.2,
      color = style_resolved$muted,
      alpha = 0.16
    )
}

totci_long <- data.table::melt(
  monitor_q[, .(qdate, `Loan stock, qavg ($bn)` = TOTCI_qavg, `YoY growth (%)` = TOTCI_yoy)],
  id.vars = "qdate",
  variable.name = "measure",
  value.name = "value"
)

p_totci <- ggplot(totci_long, aes(x = qdate, y = value, color = measure)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ measure, ncol = 1, scales = "free_y") +
  scale_color_manual(values = palette[1:2]) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "C&I loan stock",
    subtitle = "Quarterly average level and YoY growth",
    x = NULL,
    y = NULL
  )
p_totci <- add_year_guides(p_totci, totci_long, "qdate")
p_totci <- plot_reclass_line(p_totci)
p_totci <- investlabr::viz_theme_apply(
  p_totci,
  style = style,
  context = context,
  legend_position = "none",
  show_compiler = FALSE
)

standards_demand <- data.table::melt(
  monitor_q[
    ,
    .(
      qdate,
      Std_LM,
      Std_Small,
      Demand_LM,
      Demand_Small
    )
  ],
  id.vars = "qdate",
  variable.name = "measure",
  value.name = "net_pct"
)
standards_demand[
  ,
  `:=`(
    borrower = data.table::fifelse(grepl("_LM$", measure), "Large and middle-market firms", "Small firms"),
    series = data.table::fifelse(grepl("^Std_", measure), "Standards tightening", "Stronger demand")
  )
]

p_standards <- ggplot(standards_demand, aes(x = qdate, y = net_pct, color = series)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.35, color = style_resolved$muted) +
  geom_line(linewidth = 0.75, na.rm = TRUE) +
  facet_wrap(~ borrower, ncol = 1) +
  scale_color_manual(values = palette[1:2]) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "Standards versus demand",
    subtitle = "Borrower-size facets reduce overlap between SLOOS lines",
    x = NULL,
    y = "Net %"
  )
p_standards <- add_year_guides(p_standards, standards_demand, "qdate")
p_standards <- plot_reclass_line(p_standards)
p_standards <- investlabr::viz_theme_apply(
  p_standards,
  style = style,
  context = context,
  legend_position = "bottom",
  show_compiler = FALSE
)

terms_long <- data.table::melt(
  monitor_q[
    ,
    .(
      qdate,
      Terms_LM_Avg,
      Terms_Small_Avg,
      Spread_LM,
      Spread_Small
    )
  ],
  id.vars = "qdate",
  variable.name = "measure",
  value.name = "net_pct"
)
terms_long[
  ,
  `:=`(
    borrower = data.table::fifelse(grepl("_LM", measure), "Large and middle-market firms", "Small firms"),
    series = data.table::fifelse(grepl("^Terms_", measure), "Terms composite", "Loan-rate spread")
  )
]

p_terms <- ggplot(terms_long, aes(x = qdate, y = net_pct, color = series)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.35, color = style_resolved$muted) +
  geom_line(linewidth = 0.75, na.rm = TRUE) +
  facet_wrap(~ borrower, ncol = 1) +
  scale_color_manual(values = palette[c(1, 4)]) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "Loan terms tightening",
    subtitle = "Terms composite and spread component by borrower size",
    x = NULL,
    y = "Net %"
  )
p_terms <- add_year_guides(p_terms, terms_long, "qdate")
p_terms <- plot_reclass_line(p_terms)
p_terms <- investlabr::viz_theme_apply(
  p_terms,
  style = style,
  context = context,
  legend_position = "bottom",
  show_compiler = FALSE
)

recent_weekly <- monitor_w[date >= max(date, na.rm = TRUE) - 365L * 8L]
recent_weekly_long <- data.table::melt(
  recent_weekly[, .(date, `Loan stock ($bn)` = value, `YoY growth (%)` = TOTCI_yoy)],
  id.vars = "date",
  variable.name = "measure",
  value.name = "value"
)
p_weekly <- ggplot(recent_weekly_long, aes(x = date, y = value, color = measure)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.35, color = style_resolved$muted) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ measure, ncol = 1, scales = "free_y") +
  scale_color_manual(values = palette[1:2]) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Recent weekly C&I loan stock",
    subtitle = "Same stock-and-growth view as the long-history panel, focused on the latest eight years",
    x = NULL,
    y = NULL
  )
p_weekly <- plot_reclass_line(p_weekly)
p_weekly <- investlabr::viz_theme_apply(
  p_weekly,
  style = style,
  context = context,
  show_compiler = FALSE
)

latest_q <- monitor_q[
  !is.na(TOTCI_qavg) &
    (!is.na(Standards_Avg) | !is.na(Demand_Avg) | !is.na(Terms_All_Avg))
][.N]
latest_text <- sprintf(
  "Latest quarterly snapshot: %s, C&I loans = %.1f $bn, YoY = %.1f%%, standards avg = %.1f, demand avg = %.1f, terms avg = %.1f.",
  as.character(latest_q$qdate),
  latest_q$TOTCI_qavg,
  latest_q$TOTCI_yoy,
  latest_q$Standards_Avg,
  latest_q$Demand_Avg,
  latest_q$Terms_All_Avg
)

cat(
  paste0(
    "This gallery example monitors U.S. commercial-and-industrial lending conditions using local FRED cache data. ",
    "TOTCI tracks C&I loan balances, while SLOOS net-percentage series summarize bank-reported lending standards, loan demand, and loan terms. ",
    "The Jan 2025 vertical marker is included because H.8 loan categories were reclassified around that date, so changes near the break should be interpreted with care. ",
    latest_text,
    "\n\n"
  )
)

board <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_totci, p_weekly, p_standards, p_terms),
  n_rows = 2,
  n_cols = 2,
  title = "Commercial and Industrial Lending Conditions",
  bottom = paste(
    "Data source: Federal Reserve Economic Data (FRED), including TOTCI and selected SLOOS series.",
    "The dashed vertical line marks the Jan 2025 H.8 reclassification break.",
    "Positive SLOOS standards and terms values indicate net tightening; positive demand values indicate stronger demand."
  ),
  style = style,
  context = context
)

print(board)
