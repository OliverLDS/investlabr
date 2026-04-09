library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

series_id <- "DGS10"
from_date <- as.Date("2023-01-01")
horizon_days <- 20L
lookback_days <- 252L
n_sim <- 300L

# Optional sync if your local FRED cache is not up to date.
# investdatar::sync_local_fred_data(series_id)

clamp <- function(x, lower, upper) {
  min(max(x, lower), upper)
}

rate_dt <- data.table::as.data.table(investdatar::get_local_FRED_data(series_id))
if (is.null(rate_dt) || nrow(rate_dt) == 0L) {
  stop("Local FRED data not found for series: ", series_id)
}

rate_dt <- rate_dt[date >= from_date]
data.table::setorder(rate_dt, date)
rate_dt <- rate_dt[!is.na(value)]
rate_dt[, change_bp := c(NA_real_, diff(value) * 100)]
rate_dt <- rate_dt[!is.na(change_bp)]

if (nrow(rate_dt) < lookback_days) {
  stop("Need at least ", lookback_days, " observations for the shock-persistence board.")
}

recent_dt <- rate_dt[(.N - lookback_days + 1L):.N]
recent_dt[, change_lag1 := shift(change_bp)]
fit_dt <- recent_dt[!is.na(change_lag1)]

phi_raw <- stats::coef(stats::lm(change_bp ~ change_lag1, data = fit_dt))[["change_lag1"]]
phi <- clamp(as.numeric(phi_raw), -0.5, 0.8)
resid_sd <- stats::sd(fit_dt$change_bp - phi * fit_dt$change_lag1, na.rm = TRUE)
resid_sd <- max(resid_sd, 0.5)
shock_bp <- stats::sd(recent_dt$change_bp, na.rm = TRUE)
shock_bp <- max(shock_bp, 1)

sim_one_shock <- function(initial_shock_bp, seed) {
  sim_dt <- investlabr::sim_ar1_dgp(
    n = horizon_days,
    a1 = phi,
    y_lag1 = initial_shock_bp,
    sigma = resid_sd,
    drift = 0,
    burn = 100,
    seed = seed
  )
  sim_dt[, horizon := seq_len(.N)]
  sim_dt[, cum_bp := initial_shock_bp + cumsum(value)]
  sim_dt[]
}

sim_pos <- rbindlist(lapply(seq_len(n_sim), function(i) {
  dt <- sim_one_shock(shock_bp, seed = 3100 + i)
  dt[, scenario := "Positive shock"]
  dt[, path_id := i]
  dt[]
}))

sim_neg <- rbindlist(lapply(seq_len(n_sim), function(i) {
  dt <- sim_one_shock(-shock_bp, seed = 4100 + i)
  dt[, scenario := "Negative shock"]
  dt[, path_id := i]
  dt[]
}))

sim_summary <- rbindlist(list(sim_pos, sim_neg))[
  ,
  .(
    p10 = stats::quantile(cum_bp, 0.10, na.rm = TRUE),
    p50 = stats::quantile(cum_bp, 0.50, na.rm = TRUE),
    p90 = stats::quantile(cum_bp, 0.90, na.rm = TRUE)
  ),
  by = .(scenario, horizon)
]

event_threshold <- stats::quantile(abs(recent_dt$change_bp), 0.9, na.rm = TRUE)
event_idx <- which(abs(recent_dt$change_bp) >= event_threshold)
event_idx <- event_idx[event_idx + horizon_days <= nrow(recent_dt)]

realized_paths <- rbindlist(lapply(event_idx, function(idx) {
  shock_sign <- if (recent_dt$change_bp[idx] >= 0) "Positive shock" else "Negative shock"
  path_bp <- recent_dt$change_bp[idx:(idx + horizon_days)]
  data.table(
    scenario = shock_sign,
    horizon = 0:horizon_days,
    cum_bp = cumsum(path_bp),
    event_date = recent_dt$date[idx]
  )
}), fill = TRUE)

realized_summary <- realized_paths[
  ,
  .(
    mean_cum_bp = mean(cum_bp, na.rm = TRUE),
    n_events = uniqueN(event_date)
  ),
  by = .(scenario, horizon)
]

p_level <- ggplot(recent_dt, aes(date, value)) +
  geom_line(color = "#355C7D", linewidth = 1) +
  labs(
    title = "10-year Treasury yield level",
    subtitle = "Recent history for the benchmark nominal rate",
    x = NULL,
    y = "Yield (%)"
  )
p_level <- investlabr::viz_theme_apply(
  p_level,
  style = "macro_classic",
  context = "report",
  show_compiler = FALSE
)

p_changes <- ggplot(recent_dt, aes(date, change_bp)) +
  geom_hline(yintercept = c(-event_threshold, 0, event_threshold), color = c("#C5CCD4", "#8F98A3", "#C5CCD4"), linewidth = c(0.35, 0.4, 0.35)) +
  geom_col(fill = "#4E79A7", alpha = 0.85) +
  labs(
    title = "Daily yield changes",
    subtitle = "Large absolute moves define the realized shock episodes",
    x = NULL,
    y = "Change (bp)"
  )
p_changes <- investlabr::viz_theme_apply(
  p_changes,
  style = "macro_classic",
  context = "report",
  show_compiler = FALSE
)

p_sim <- ggplot(sim_summary, aes(horizon, p50, color = scenario, fill = scenario)) +
  geom_hline(yintercept = 0, color = "#9AA4AF", linewidth = 0.35) +
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1) +
  labs(
    title = "Simulated shock-persistence envelope",
    subtitle = "AR-style daily change dynamics calibrated on recent data",
    x = "Trading days after shock",
    y = "Cumulative level effect (bp)",
    color = NULL,
    fill = NULL
  )
p_sim <- investlabr::viz_theme_apply(
  p_sim,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

p_realized <- ggplot(realized_summary, aes(horizon, mean_cum_bp, color = scenario)) +
  geom_hline(yintercept = 0, color = "#9AA4AF", linewidth = 0.35) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average realized path after large shock days",
    subtitle = "Historical follow-through for the biggest recent moves",
    x = "Trading days after shock",
    y = "Average cumulative level effect (bp)",
    color = NULL
  )
p_realized <- investlabr::viz_theme_apply(
  p_realized,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_level, p_changes, p_sim, p_realized),
  n_rows = 2,
  n_cols = 2,
  title = "10-year Treasury Shock Persistence",
  bottom = paste(
    "Source: Federal Reserve Economic Data (FRED).",
    paste0(
      "The board uses the most recent ", lookback_days, " trading-day-equivalent observations through ",
      format(max(recent_dt$date), "%Y-%m-%d"), "."
    ),
    paste0(
      "Daily rate shocks are measured in basis points, with a one-standard-deviation move near ",
      sprintf("%.1f", shock_bp), " bp."
    ),
    paste0(
      "The persistence benchmark is an AR(1)-style fit on daily changes with phi = ",
      sprintf("%.2f", phi),
      "; the realized panel averages historical episodes above the 90th percentile of absolute daily moves."
    )
  ),
  style = "macro_classic",
  context = "report",
  show_compiler = TRUE
)
