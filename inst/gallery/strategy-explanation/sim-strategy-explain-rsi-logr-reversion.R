# Purpose: Explain the log-return RSI reversion rule used by the real-data log-return RSI backtest.
# Data source: Simulated OHLC data.
# Main functions demonstrated: strategyr::strat_rsi_logr_revert_tgt_pos(), investlabr visualization styling.
# Promotion candidate: Strategy explanation panels may later become investlabr viz helpers backed by strategyr diagnostics.

library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(46)

style <- "strategy_explain"
context <- "report"
target_size <- 0.95
rsi_h <- 18
oversold <- 40
overbought <- 65
exit_level <- 47.5
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

n_obs <- 190L
log_ret <- c(
  stats::rnorm(45, -0.006, 0.010),
  stats::rnorm(45, 0.010, 0.012),
  stats::rnorm(48, -0.008, 0.011),
  stats::rnorm(52, 0.005, 0.010)
)
close <- 100 * exp(cumsum(log_ret))

datetime <- as.POSIXct(as.Date("2026-01-01") + seq_along(close) - 1L, tz = "UTC")
open <- data.table::shift(close, n = 1L, type = "lag", fill = close[1]) *
  exp(stats::rnorm(length(close), 0, 0.002))
high <- pmax(open, close) * exp(stats::runif(length(close), 0.002, 0.008))
low <- pmin(open, close) * exp(-stats::runif(length(close), 0.002, 0.008))

DT <- data.table::data.table(datetime, open, high, low, close)
target <- strategyr::strat_rsi_logr_revert_tgt_pos(
  DT,
  h = rsi_h,
  oversold = oversold,
  overbought = overbought,
  exit_level = exit_level,
  target_size = target_size,
  compute_features = TRUE
)
rsi_col <- paste0("rsi_logr_", rsi_h)
DT[, `:=`(rsi_logr = .SD[[rsi_col]], target = target)]
DT[, log_ret := c(NA_real_, diff(log(close)))]

DT[, prev_target := data.table::shift(target, n = 1L, fill = 0)]
events <- DT[target != prev_target]
events[, signal := data.table::fifelse(
  target > 0,
  "Enter long: log-return RSI oversold",
  data.table::fifelse(target < 0, "Enter short: log-return RSI overbought", "Exit: indicator mean-reverted")
)]

p_price <- investlabr::viz_candle_base(DT, style = style, context = context, show_compiler = FALSE) +
  geom_vline(
    data = events,
    aes(xintercept = datetime),
    linetype = "dashed",
    linewidth = 0.35,
    color = resolved$accent,
    alpha = 0.45
  ) +
  labs(
    title = "Log-return RSI reversion on a simulated price path",
    subtitle = sprintf("The real-data example uses half-life h = %s, oversold = %s, overbought = %s, exit = %s.", rsi_h, oversold, overbought, exit_level),
    x = NULL,
    y = "Simulated price"
  )
p_price <- investlabr::viz_theme_apply(p_price, style = style, context = context, show_compiler = FALSE)

p_rsi <- ggplot(DT, aes(x = datetime, y = rsi_logr)) +
  annotate("rect", xmin = min(DT$datetime), xmax = max(DT$datetime), ymin = -Inf, ymax = oversold, fill = scales::alpha(palette[3], 0.12), color = NA) +
  annotate("rect", xmin = min(DT$datetime), xmax = max(DT$datetime), ymin = overbought, ymax = Inf, fill = scales::alpha(palette[4], 0.10), color = NA) +
  geom_hline(yintercept = c(oversold, overbought), linetype = "dashed", linewidth = 0.4, color = resolved$muted) +
  geom_hline(yintercept = exit_level, linetype = "dotted", linewidth = 0.45, color = palette[2]) +
  geom_line(color = palette[1], linewidth = 0.85, na.rm = TRUE) +
  geom_point(data = events, aes(y = rsi_logr), color = resolved$accent, size = 2.2, na.rm = TRUE) +
  ggrepel::geom_label_repel(data = events, aes(label = signal), size = 3, min.segment.length = 0, seed = 46, show.legend = FALSE, na.rm = TRUE) +
  scale_y_continuous(limits = c(0, 100), breaks = c(oversold, exit_level, overbought)) +
  labs(
    title = "Log-return RSI signal zones",
    subtitle = "This version smooths positive and negative log-return momentum rather than price-level changes.",
    x = NULL,
    y = "Log-return RSI"
  )
p_rsi <- investlabr::viz_theme_apply(p_rsi, style = style, context = context, show_compiler = FALSE)

p_target <- ggplot(DT, aes(x = datetime, y = target)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dotted", color = resolved$muted) +
  geom_step(color = palette[1], linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) +
  labs(
    title = "Target exposure",
    subtitle = "The thresholds are closer to the center than classic 30/70 RSI because this indicator is based on smoothed log returns.",
    x = NULL,
    y = NULL
  )
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, show_compiler = FALSE)

cat(
  paste0(
    "This simulated strategy-explanation example mirrors real-data-strategyr-rsi-logr-backtest.R. ",
    "Log-return RSI uses smoothed positive and negative log-return momentum with half-life h = ", rsi_h, ". ",
    "Below ", oversold, " targets long exposure, above ", overbought, " targets short exposure, and ",
    "mean reversion to ", exit_level, " flattens the open position.\n\n"
  )
)

board <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_price, p_rsi, p_target),
  n_rows = 3,
  n_cols = 1,
  title = "How Log-Return RSI Reversion Converts Momentum Extremes Into Exposure",
  bottom = "Simulated OHLC data. The indicator thresholds match the real-data log-return RSI gallery example.",
  style = style,
  context = context
)

print(board)
