library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

ticker <- "SPY"
from_date <- as.Date("2024-01-01")
to_date <- Sys.Date()
sample_days_target <- 252L
roll_window <- 21L

# Optional sync if your local Yahoo cache is not up to date.
# investdatar::sync_local_quantmod_OHLC(
#   ticker = ticker,
#   from = as.character(from_date),
#   to = as.character(to_date),
#   src = "yahoo"
# )

clamp <- function(x, lower, upper) {
  min(max(x, lower), upper)
}

calc_sq_acf_dt <- function(x, label, lag_max = 20L) {
  acf_obj <- stats::acf(x^2, lag.max = lag_max, plot = FALSE, na.action = na.pass)
  data.table(
    lag = seq_len(lag_max),
    acf = as.numeric(acf_obj$acf[-1]),
    series = label
  )
}

ohlc_dt <- data.table::as.data.table(
  investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo")
)

if (is.null(ohlc_dt) || nrow(ohlc_dt) == 0L) {
  stop("Local Yahoo data not found for ticker: ", ticker)
}

ohlc_dt <- ohlc_dt[datetime >= as.POSIXct(from_date) & datetime < as.POSIXct(to_date + 1)]
data.table::setorder(ohlc_dt, datetime)
ohlc_dt[, log_ret := c(NA_real_, diff(log(close)))]
ret_dt <- ohlc_dt[!is.na(log_ret), .(datetime, close, log_ret)]

sample_days <- min(sample_days_target, nrow(ret_dt))
if (sample_days < 126L) {
  stop("Need at least 126 return observations for this volatility-clustering board.")
}

sample_dt <- ret_dt[(.N - sample_days + 1L):.N]
sample_dt[, centered_ret := log_ret - mean(log_ret, na.rm = TRUE)]
sample_dt[, abs_ret := abs(centered_ret)]
sample_dt[, sq_ret := centered_ret^2]
sample_dt[, ann_vol := sqrt(252) * sqrt(data.table::frollmean(sq_ret, n = roll_window, align = "right"))]
sample_dt[, index100 := 100 * exp(cumsum(log_ret))]

ret_mean <- mean(sample_dt$log_ret, na.rm = TRUE)
ret_sd <- stats::sd(sample_dt$log_ret, na.rm = TRUE)
sq_ret <- sample_dt$sq_ret
sq_corr_raw <- suppressWarnings(stats::cor(sq_ret[-1], sq_ret[-length(sq_ret)], use = "complete.obs"))
if (!is.finite(sq_corr_raw)) sq_corr_raw <- 0.8
sq_corr <- clamp(sq_corr_raw, 0.7, 0.95)

alpha1 <- clamp(0.12 * sq_corr / 0.8, 0.05, 0.18)
beta1 <- clamp(sq_corr - alpha1, 0.7, 0.92)
if ((alpha1 + beta1) >= 0.98) beta1 <- 0.98 - alpha1
alpha0 <- stats::var(sample_dt$log_ret, na.rm = TRUE) * (1 - alpha1 - beta1)
alpha0 <- max(alpha0, 1e-08)

iid_sim <- investlabr::sim_ar1_dgp(
  n = nrow(sample_dt),
  a1 = 0,
  sigma = ret_sd,
  drift = ret_mean,
  burn = 100,
  seed = 20260410
)

garch_sim <- investlabr::sim_garch11_dgp(
  n = nrow(sample_dt),
  alpha0 = alpha0,
  alpha1 = alpha1,
  beta1 = beta1,
  burn = 150,
  seed = 20260411
)
garch_sim[, value := value + ret_mean]

sim_dt <- data.table(
  datetime = sample_dt$datetime,
  actual = sample_dt$log_ret,
  iid = iid_sim$value,
  garch = garch_sim$value
)

path_dt <- melt(
  copy(sim_dt)[, `:=`(
    actual = 100 * exp(cumsum(actual)),
    iid = 100 * exp(cumsum(iid)),
    garch = 100 * exp(cumsum(garch))
  )],
  id.vars = "datetime",
  variable.name = "series",
  value.name = "index100"
)
path_dt[, series := factor(series, levels = c("actual", "iid", "garch"), labels = c("Actual", "IID benchmark", "GARCH benchmark"))]

vol_dt <- melt(
  copy(sim_dt)[, `:=`(
    actual = sqrt(252) * sqrt(data.table::frollmean(actual^2, n = roll_window, align = "right")),
    iid = sqrt(252) * sqrt(data.table::frollmean(iid^2, n = roll_window, align = "right")),
    garch = sqrt(252) * sqrt(data.table::frollmean(garch^2, n = roll_window, align = "right"))
  )],
  id.vars = "datetime",
  variable.name = "series",
  value.name = "ann_vol"
)
vol_dt[, series := factor(series, levels = c("actual", "iid", "garch"), labels = c("Actual", "IID benchmark", "GARCH benchmark"))]

shock_dt <- melt(
  copy(sim_dt)[, `:=`(
    actual = abs(actual),
    iid = abs(iid),
    garch = abs(garch)
  )],
  id.vars = "datetime",
  variable.name = "series",
  value.name = "abs_ret"
)
shock_dt[, series := factor(series, levels = c("actual", "iid", "garch"), labels = c("Actual", "IID benchmark", "GARCH benchmark"))]

acf_dt <- rbindlist(list(
  calc_sq_acf_dt(sample_dt$log_ret, "Actual"),
  calc_sq_acf_dt(iid_sim$value, "IID benchmark"),
  calc_sq_acf_dt(garch_sim$value, "GARCH benchmark")
))
acf_dt[, series := factor(series, levels = c("Actual", "IID benchmark", "GARCH benchmark"))]

p_path <- ggplot(path_dt, aes(datetime, index100, color = series)) +
  geom_line(linewidth = 0.95) +
  labs(
    title = "Indexed path over the recent sample",
    subtitle = "Actual market history versus simple simulated benchmarks",
    x = NULL,
    y = "Index (start = 100)",
    color = NULL
  )
p_path <- investlabr::viz_theme_apply(
  p_path,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

p_vol <- ggplot(vol_dt, aes(datetime, ann_vol, color = series)) +
  geom_line(linewidth = 0.95, na.rm = TRUE) +
  labs(
    title = "Rolling annualized volatility",
    subtitle = paste0(roll_window, "-day window"),
    x = NULL,
    y = "Volatility",
    color = NULL
  )
p_vol <- investlabr::viz_theme_apply(
  p_vol,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

p_shock <- ggplot(shock_dt, aes(datetime, abs_ret, color = series)) +
  geom_line(linewidth = 0.8, alpha = 0.95) +
  labs(
    title = "Absolute daily returns",
    subtitle = "Volatility clustering shows up as bursts of larger moves",
    x = NULL,
    y = "|Daily log return|",
    color = NULL
  )
p_shock <- investlabr::viz_theme_apply(
  p_shock,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

p_acf <- ggplot(acf_dt, aes(lag, acf, color = series)) +
  geom_hline(yintercept = 0, color = "#9AA4AF", linewidth = 0.35) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 1.8) +
  labs(
    title = "Autocorrelation of squared returns",
    subtitle = "A simple clustering diagnostic",
    x = "Lag (days)",
    y = "ACF of squared returns",
    color = NULL
  )
p_acf <- investlabr::viz_theme_apply(
  p_acf,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_path, p_vol, p_shock, p_acf),
  n_rows = 2,
  n_cols = 2,
  title = paste0(ticker, ": volatility clustering board"),
  bottom = paste(
    "Source: Yahoo Finance.",
    paste0(
      "The actual series uses the latest ", sample_days, " trading days through ", format(max(as.Date(sample_dt$datetime)), "%Y-%m-%d"), "."
    ),
    paste0(
      "The iid benchmark matches the recent mean and volatility but assumes no clustering."
    ),
    paste0(
      "The GARCH benchmark is calibrated from the recent return sample with alpha1 = ", sprintf("%.2f", alpha1),
      ", beta1 = ", sprintf("%.2f", beta1), ", and unconditional daily volatility near ",
      sprintf("%.2f%%", 100 * sqrt(alpha0 / (1 - alpha1 - beta1))), "."
    )
  ),
  style = "macro_classic",
  context = "report",
  show_compiler = TRUE
)
