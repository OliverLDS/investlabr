# Purpose: Explain the classic RSI reversion rule used by the real-data RSI backtest.
# Data source: Simulated OHLC data.
# Main functions demonstrated: strategyr::strat_rsi_revert_tgt_pos(), investlabr visualization styling.
# Promotion candidate: Strategy explanation panels may later become investlabr viz helpers backed by strategyr diagnostics.

library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(45)

style <- "strategy_explain"
context <- "report"
target_size <- 0.95
rsi_n <- 21L
oversold <- 25
overbought <- 75
exit_level <- 45
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

n_obs <- 180L
close <- c(
  seq(102, 91, length.out = 34),
  seq(91, 108, length.out = 36),
  seq(108, 123, length.out = 34),
  seq(123, 103, length.out = 42),
  seq(103, 112, length.out = 34)
) + 1.2 * sin(seq_len(n_obs) / 3.7) + stats::rnorm(n_obs, 0, 0.45)

datetime <- as.POSIXct(as.Date("2026-01-01") + seq_along(close) - 1L, tz = "UTC")
open <- data.table::shift(close, n = 1L, type = "lag", fill = close[1]) +
  stats::rnorm(length(close), 0, 0.25)
high <- pmax(open, close) + stats::runif(length(close), 0.2, 0.9)
low <- pmin(open, close) - stats::runif(length(close), 0.2, 0.9)

DT <- data.table::data.table(datetime, open, high, low, close)
target <- strategyr::strat_rsi_revert_tgt_pos(
  DT,
  n = rsi_n,
  oversold = oversold,
  overbought = overbought,
  exit_level = exit_level,
  target_size = target_size,
  compute_features = TRUE
)
rsi_col <- paste0("rsi_", rsi_n)
DT[, `:=`(rsi = .SD[[rsi_col]], target = target)]

DT[, prev_target := data.table::shift(target, n = 1L, fill = 0)]
events <- DT[target != prev_target]
events[, signal := data.table::fifelse(
  target > 0,
  "Enter long: RSI oversold",
  data.table::fifelse(target < 0, "Enter short: RSI overbought", "Exit: RSI mean-reverted")
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
    title = "Classic RSI reversion on a simulated price path",
    subtitle = sprintf("The real-data example uses RSI n = %s, oversold = %s, overbought = %s, exit = %s.", rsi_n, oversold, overbought, exit_level),
    x = NULL,
    y = "Simulated price"
  )
p_price <- investlabr::viz_theme_apply(p_price, style = style, context = context, show_compiler = FALSE)

p_rsi <- ggplot(DT, aes(x = datetime, y = rsi)) +
  annotate(
    "rect",
    xmin = min(DT$datetime),
    xmax = max(DT$datetime),
    ymin = -Inf,
    ymax = oversold,
    fill = scales::alpha(palette[3], 0.12),
    color = NA
  ) +
  annotate(
    "rect",
    xmin = min(DT$datetime),
    xmax = max(DT$datetime),
    ymin = overbought,
    ymax = Inf,
    fill = scales::alpha(palette[4], 0.10),
    color = NA
  ) +
  geom_hline(yintercept = c(oversold, overbought), linetype = "dashed", linewidth = 0.4, color = resolved$muted) +
  geom_hline(yintercept = exit_level, linetype = "dotted", linewidth = 0.45, color = palette[2]) +
  geom_line(color = palette[1], linewidth = 0.85, na.rm = TRUE) +
  geom_point(data = events, aes(y = rsi), color = resolved$accent, size = 2.2, na.rm = TRUE) +
  ggrepel::geom_label_repel(
    data = events,
    aes(label = signal),
    size = 3,
    min.segment.length = 0,
    seed = 45,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = c(oversold, exit_level, overbought)) +
  labs(
    title = "RSI signal zones",
    subtitle = "Oversold opens long, overbought opens short, and the exit level flattens an open position.",
    x = NULL,
    y = "RSI"
  )
p_rsi <- investlabr::viz_theme_apply(p_rsi, style = style, context = context, show_compiler = FALSE)

p_target <- ggplot(DT, aes(x = datetime, y = target)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dotted", color = resolved$muted) +
  geom_step(color = palette[1], linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(
    breaks = c(-target_size, 0, target_size),
    labels = c("Short", "Flat", "Long"),
    limits = c(-1.05, 1.05)
  ) +
  labs(
    title = "Target exposure",
    subtitle = "The rule is stateful: an open long or short remains until the RSI exit condition is met.",
    x = NULL,
    y = NULL
  )
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, show_compiler = FALSE)

cat(
  paste0(
    "This simulated strategy-explanation example mirrors real-data-strategyr-rsi-backtest.R. ",
    "The classic RSI rule treats RSI below ", oversold, " as oversold and targets long exposure, ",
    "RSI above ", overbought, " as overbought and targets short exposure, and RSI crossing the ",
    exit_level, " exit level as a reason to flatten the open position.\n\n"
  )
)

board <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_price, p_rsi, p_target),
  n_rows = 3,
  n_cols = 1,
  title = "How Classic RSI Reversion Converts Extremes Into Exposure",
  bottom = "Simulated OHLC data. The indicator thresholds match the real-data RSI gallery example.",
  style = style,
  context = context
)

print(board)
