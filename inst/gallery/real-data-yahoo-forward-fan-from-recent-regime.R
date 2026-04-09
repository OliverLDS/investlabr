library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

ticker <- "^GSPC"
sample_days <- 252L
horizon_days <- 60L
n_paths <- 400L
roll_window <- 21L

# Optional sync if your local Yahoo cache is not up to date.
# investdatar::sync_local_quantmod_OHLC(
#   ticker = ticker,
#   from = "2020-01-01",
#   to = as.character(Sys.Date()),
#   src = "yahoo"
# )

clamp <- function(x, lower, upper) {
  min(max(x, lower), upper)
}

compute_drawdown <- function(path_values) {
  running_peak <- cummax(path_values)
  path_values / running_peak - 1
}

ohlc_dt <- data.table::as.data.table(
  investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo")
)
if (is.null(ohlc_dt) || nrow(ohlc_dt) == 0L) {
  stop("Local Yahoo data not found for ticker: ", ticker)
}

data.table::setorder(ohlc_dt, datetime)
ohlc_dt[, log_ret := c(NA_real_, diff(log(close)))]
ret_dt <- ohlc_dt[!is.na(log_ret), .(datetime, close, log_ret)]

if (nrow(ret_dt) < sample_days) {
  stop("Need at least ", sample_days, " return observations for the forward-fan board.")
}

sample_dt <- ret_dt[(.N - sample_days + 1L):.N]
recent_history_dt <- ret_dt[(.N - 125L):.N]
ret_mean <- mean(sample_dt$log_ret, na.rm = TRUE)
ret_var <- stats::var(sample_dt$log_ret, na.rm = TRUE)
sq_ret <- sample_dt$log_ret^2
sq_corr_raw <- suppressWarnings(stats::cor(sq_ret[-1], sq_ret[-length(sq_ret)], use = "complete.obs"))
if (!is.finite(sq_corr_raw)) sq_corr_raw <- 0.82
sq_corr <- clamp(sq_corr_raw, 0.72, 0.96)

alpha1 <- clamp(0.12 * sq_corr / 0.8, 0.05, 0.18)
beta1 <- clamp(sq_corr - alpha1, 0.72, 0.93)
if ((alpha1 + beta1) >= 0.985) beta1 <- 0.985 - alpha1
alpha0 <- max(ret_var * (1 - alpha1 - beta1), 1e-08)
sigma2_0 <- tail(data.table::frollmean(sample_dt$log_ret^2, n = roll_window, align = "right"), 1)
if (!is.finite(sigma2_0) || sigma2_0 <= 0) sigma2_0 <- ret_var

last_close <- tail(sample_dt$close, 1)
last_date <- as.Date(tail(sample_dt$datetime, 1))
future_dates <- seq(last_date + 1, by = "day", length.out = horizon_days * 2L)
future_dates <- future_dates[weekdays(future_dates) %chin% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
future_dates <- future_dates[seq_len(horizon_days)]

sim_paths <- rbindlist(lapply(seq_len(n_paths), function(i) {
  sim_dt <- investlabr::sim_garch11_dgp(
    n = horizon_days,
    alpha0 = alpha0,
    alpha1 = alpha1,
    beta1 = beta1,
    sigma2_0 = sigma2_0,
    burn = 150,
    seed = 5100 + i
  )
  sim_dt[, ret := value + ret_mean]
  sim_dt[, price := last_close * exp(cumsum(ret))]
  sim_dt[, horizon := seq_len(.N)]
  sim_dt[, path_id := i]
  sim_dt[, drawdown := compute_drawdown(price)]
  sim_dt[]
}))

fan_dt <- sim_paths[
  ,
  .(
    p10 = stats::quantile(price, 0.10, na.rm = TRUE),
    p25 = stats::quantile(price, 0.25, na.rm = TRUE),
    p50 = stats::quantile(price, 0.50, na.rm = TRUE),
    p75 = stats::quantile(price, 0.75, na.rm = TRUE),
    p90 = stats::quantile(price, 0.90, na.rm = TRUE)
  ),
  by = horizon
]
fan_dt[, datetime := future_dates[horizon]]

sample_paths <- sim_paths[path_id %in% c(1L, 2L, 3L, 4L, 5L), .(horizon, path_id, price)]
sample_paths[, datetime := future_dates[horizon]]

history_plot_dt <- recent_history_dt[, .(datetime, price = close)]

terminal_dt <- sim_paths[horizon == max(horizon), .(
  terminal_return = price / last_close - 1,
  max_drawdown = min(drawdown, na.rm = TRUE)
), by = path_id]

vol_history_dt <- sample_dt[, .(
  datetime,
  realized_vol = sqrt(252) * sqrt(data.table::frollmean(log_ret^2, n = roll_window, align = "right"))
)]

vol_sim_dt <- sim_paths[, .(
  horizon,
  ann_vol = sqrt(252 * sigma2)
)]
vol_sim_summary <- vol_sim_dt[, .(
  p10 = stats::quantile(ann_vol, 0.10, na.rm = TRUE),
  p50 = stats::quantile(ann_vol, 0.50, na.rm = TRUE),
  p90 = stats::quantile(ann_vol, 0.90, na.rm = TRUE)
), by = horizon]
vol_sim_summary[, datetime := future_dates[horizon]]

p_fan <- ggplot() +
  geom_line(data = history_plot_dt, aes(datetime, price), color = "#355C7D", linewidth = 1) +
  geom_ribbon(data = fan_dt, aes(datetime, ymin = p10, ymax = p90), fill = "#8FB9E4", alpha = 0.20) +
  geom_ribbon(data = fan_dt, aes(datetime, ymin = p25, ymax = p75), fill = "#5B8DB8", alpha = 0.28) +
  geom_line(data = fan_dt, aes(datetime, p50), color = "#1F4E79", linewidth = 1) +
  labs(
    title = "Recent history and forward fan",
    subtitle = "Median and percentile bands from recent-regime simulations",
    x = NULL,
    y = "Index level"
  )
p_fan <- investlabr::viz_theme_apply(
  p_fan,
  style = "macro_classic",
  context = "report",
  show_compiler = FALSE
)

p_paths <- ggplot(sample_paths, aes(datetime, price, group = path_id, color = factor(path_id))) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  labs(
    title = "Sample forward paths",
    subtitle = "Five simulated continuations from the current regime",
    x = NULL,
    y = "Index level",
    color = NULL
  )
p_paths <- investlabr::viz_theme_apply(
  p_paths,
  style = "macro_classic",
  context = "report",
  legend_position = "none",
  show_compiler = FALSE
)

p_terminal <- ggplot(terminal_dt, aes(terminal_return)) +
  geom_histogram(bins = 30, fill = "#4E79A7", color = "white", alpha = 0.9) +
  labs(
    title = "Terminal return distribution",
    subtitle = paste0(horizon_days, "-day horizon"),
    x = "Terminal return",
    y = "Count"
  )
p_terminal <- investlabr::viz_theme_apply(
  p_terminal,
  style = "macro_classic",
  context = "report",
  show_compiler = FALSE
)

p_dd <- ggplot() +
  geom_line(data = vol_history_dt, aes(datetime, realized_vol), color = "#7A8C55", linewidth = 1, na.rm = TRUE) +
  geom_ribbon(data = vol_sim_summary, aes(datetime, ymin = p10, ymax = p90), fill = "#A9C18D", alpha = 0.20) +
  geom_line(data = vol_sim_summary, aes(datetime, p50), color = "#56723D", linewidth = 1) +
  labs(
    title = "Volatility carry-over",
    subtitle = "Recent realized volatility and the simulated forward envelope",
    x = NULL,
    y = "Annualized volatility"
  )
p_dd <- investlabr::viz_theme_apply(
  p_dd,
  style = "macro_classic",
  context = "report",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_fan, p_paths, p_terminal, p_dd),
  n_rows = 2,
  n_cols = 2,
  title = paste0(ticker, ": forward fan from the recent regime"),
  bottom = paste(
    "Source: Yahoo Finance.",
    paste0(
      "The calibration window uses the latest ", sample_days, " trading days through ",
      format(last_date, "%Y-%m-%d"), "."
    ),
    paste0(
      "The forward fan uses ", n_paths, " GARCH-style paths over ", horizon_days,
      " trading days, with alpha1 = ", sprintf("%.2f", alpha1),
      " and beta1 = ", sprintf("%.2f", beta1), "."
    ),
    "This is a regime-consistent uncertainty illustration, not a point forecast."
  ),
  style = "macro_classic",
  context = "report",
  show_compiler = TRUE
)
