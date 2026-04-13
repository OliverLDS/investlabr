# Purpose: Explain the MACD cross and MACD contrarian rules used by the real-data MACD backtest.
# Data source: Simulated OHLC data.
# Main functions demonstrated: strategyr::strat_macd_cross_tgt_pos(), strategyr::strat_macd_contrarian_tgt_pos(), investlabr visualization styling.
# Promotion candidate: Strategy explanation panels may later become investlabr viz helpers backed by strategyr diagnostics.

library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(44)

style <- "strategy_explain"
context <- "report"
target_size <- 0.95
cross_fast <- 10L
cross_slow <- 35L
cross_signal <- 12L
contrarian_fast <- 10L
contrarian_slow <- 35L
contrarian_signal <- 7L
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

# Stylized path: early trend, drawdown, recovery, and late fade. The goal is to
# generate multiple MACD spread sign changes for explanation, not realism.
n_obs <- 180L
trend_piece <- c(
  seq(100, 113, length.out = 45),
  seq(113, 96, length.out = 45),
  seq(96, 121, length.out = 55),
  seq(121, 108, length.out = 35)
)
cycle_piece <- 1.8 * sin(seq_len(n_obs) / 5.2) + 0.8 * sin(seq_len(n_obs) / 13)
close <- trend_piece + cycle_piece + stats::rnorm(n_obs, 0, 0.35)

datetime <- as.POSIXct(as.Date("2026-01-01") + seq_along(close) - 1L, tz = "UTC")
open <- data.table::shift(close, n = 1L, type = "lag", fill = close[1]) +
  stats::rnorm(length(close), 0, 0.25)
high <- pmax(open, close) + stats::runif(length(close), 0.2, 0.9)
low <- pmin(open, close) - stats::runif(length(close), 0.2, 0.9)

DT <- data.table::data.table(
  datetime = datetime,
  open = open,
  high = high,
  low = low,
  close = close
)

cross_tgt <- strategyr::strat_macd_cross_tgt_pos(
  DT,
  fast = cross_fast,
  slow = cross_slow,
  signal = cross_signal,
  target_size = target_size,
  compute_features = TRUE
)
contrarian_tgt <- strategyr::strat_macd_contrarian_tgt_pos(
  DT,
  fast = contrarian_fast,
  slow = contrarian_slow,
  signal = contrarian_signal,
  target_size = target_size,
  compute_features = TRUE
)

cross_macd_col <- paste0("macd_", cross_fast, "_", cross_slow)
cross_signal_col <- paste0("macd_signal_", cross_fast, "_", cross_slow, "_", cross_signal)
contrarian_macd_col <- paste0("macd_", contrarian_fast, "_", contrarian_slow)
contrarian_signal_col <- paste0("macd_signal_", contrarian_fast, "_", contrarian_slow, "_", contrarian_signal)

DT[, `:=`(
  cross_macd = .SD[[cross_macd_col]],
  cross_signal = .SD[[cross_signal_col]],
  cross_spread = .SD[[cross_macd_col]] - .SD[[cross_signal_col]],
  cross_target = cross_tgt,
  contrarian_macd = .SD[[contrarian_macd_col]],
  contrarian_signal = .SD[[contrarian_signal_col]],
  contrarian_spread = .SD[[contrarian_macd_col]] - .SD[[contrarian_signal_col]],
  contrarian_target = contrarian_tgt
)]

find_signal_changes <- function(dt, target_col, spread_col, label_prefix) {
  out <- data.table::copy(dt)
  out[, target := get(target_col)]
  out[, spread := get(spread_col)]
  out[, prev_target := data.table::shift(target, n = 1L, fill = 0)]
  out <- out[is.finite(spread) & target != prev_target]
  out[, signal := data.table::fifelse(target > 0, paste(label_prefix, "long"), paste(label_prefix, "short"))]
  out
}

cross_events <- find_signal_changes(DT, "cross_target", "cross_spread", "Cross")
contrarian_events <- find_signal_changes(DT, "contrarian_target", "contrarian_spread", "Contrarian")

p_price <- investlabr::viz_candle_base(
  DT,
  style = style,
  context = context,
  show_compiler = FALSE
) +
  geom_vline(
    data = cross_events,
    aes(xintercept = datetime),
    linetype = "dashed",
    linewidth = 0.35,
    color = palette[1],
    alpha = 0.45
  ) +
  geom_vline(
    data = contrarian_events,
    aes(xintercept = datetime),
    linetype = "dotted",
    linewidth = 0.35,
    color = palette[2],
    alpha = 0.45
  ) +
  labs(
    title = "Simulated price path for MACD-family strategies",
    subtitle = "Dashed lines mark MACD-cross target changes; dotted lines mark contrarian target changes.",
    x = NULL,
    y = "Simulated price"
  )
p_price <- investlabr::viz_theme_apply(
  p_price,
  style = style,
  context = context,
  legend_position = "bottom",
  show_compiler = FALSE
)

cross_long <- data.table::melt(
  DT[, .(datetime, `MACD line` = cross_macd, `Signal line` = cross_signal)],
  id.vars = "datetime",
  variable.name = "series",
  value.name = "value"
)
cross_hist <- DT[is.finite(cross_spread), .(
  datetime,
  spread = cross_spread,
  sign = data.table::fifelse(cross_spread >= 0, "Positive spread", "Negative spread")
)]

p_cross <- ggplot() +
  geom_col(
    data = cross_hist,
    aes(x = datetime, y = spread, fill = sign),
    alpha = 0.35,
    width = 0.8
  ) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dotted", color = resolved$muted) +
  geom_line(
    data = cross_long,
    aes(x = datetime, y = value, color = series),
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_vline(
    data = cross_events,
    aes(xintercept = datetime),
    linetype = "dashed",
    linewidth = 0.4,
    color = palette[1],
    alpha = 0.6
  ) +
  scale_color_manual(values = c("MACD line" = palette[1], "Signal line" = palette[2])) +
  scale_fill_manual(values = c("Positive spread" = scales::alpha(palette[3], 0.65), "Negative spread" = scales::alpha(palette[4], 0.65))) +
  labs(
    title = sprintf("MACD cross indicator: %s/%s/%s", cross_fast, cross_slow, cross_signal),
    subtitle = "MACD above signal means positive spread and long target; MACD below signal means short target.",
    x = NULL,
    y = "MACD spread",
    color = NULL,
    fill = NULL
  )
p_cross <- investlabr::viz_theme_apply(
  p_cross,
  style = style,
  context = context,
  legend_position = "bottom",
  show_compiler = FALSE
)

contrarian_long <- data.table::melt(
  DT[, .(datetime, `MACD line` = contrarian_macd, `Signal line` = contrarian_signal)],
  id.vars = "datetime",
  variable.name = "series",
  value.name = "value"
)
contrarian_hist <- DT[is.finite(contrarian_spread), .(
  datetime,
  spread = contrarian_spread,
  sign = data.table::fifelse(contrarian_spread >= 0, "Positive spread", "Negative spread")
)]

p_contrarian <- ggplot() +
  geom_col(
    data = contrarian_hist,
    aes(x = datetime, y = spread, fill = sign),
    alpha = 0.35,
    width = 0.8
  ) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dotted", color = resolved$muted) +
  geom_line(
    data = contrarian_long,
    aes(x = datetime, y = value, color = series),
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_vline(
    data = contrarian_events,
    aes(xintercept = datetime),
    linetype = "dotted",
    linewidth = 0.4,
    color = palette[2],
    alpha = 0.6
  ) +
  scale_color_manual(values = c("MACD line" = palette[1], "Signal line" = palette[2])) +
  scale_fill_manual(values = c("Positive spread" = scales::alpha(palette[3], 0.65), "Negative spread" = scales::alpha(palette[4], 0.65))) +
  labs(
    title = sprintf("MACD contrarian indicator: %s/%s/%s", contrarian_fast, contrarian_slow, contrarian_signal),
    subtitle = "The contrarian rule flips the MACD-cross interpretation: positive spread targets short, negative spread targets long.",
    x = NULL,
    y = "MACD spread",
    color = NULL,
    fill = NULL
  )
p_contrarian <- investlabr::viz_theme_apply(
  p_contrarian,
  style = style,
  context = context,
  legend_position = "bottom",
  show_compiler = FALSE
)

target_long <- data.table::melt(
  DT[
    ,
    .(
      datetime,
      `MACD cross target` = cross_target,
      `MACD contrarian target` = contrarian_target
    )
  ],
  id.vars = "datetime",
  variable.name = "strategy",
  value.name = "target"
)

p_target <- ggplot(target_long, aes(x = datetime, y = target, color = strategy)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dotted", color = resolved$muted) +
  geom_step(linewidth = 0.85, na.rm = TRUE) +
  scale_color_manual(values = c("MACD cross target" = palette[1], "MACD contrarian target" = palette[2])) +
  scale_y_continuous(
    breaks = c(-target_size, 0, target_size),
    labels = c("Short", "Flat", "Long"),
    limits = c(-1.05, 1.05)
  ) +
  labs(
    title = "Target exposure comparison",
    subtitle = "Both examples use target size 0.95, but the contrarian rule intentionally reverses the MACD-cross direction.",
    x = NULL,
    y = NULL,
    color = NULL
  )
p_target <- investlabr::viz_theme_apply(
  p_target,
  style = style,
  context = context,
  legend_position = "bottom",
  show_compiler = FALSE
)

cat(
  paste0(
    "This simulated strategy-explanation example mirrors the two MACD-family strategies used in ",
    "real-data-strategyr-macd-backtest.R. MACD cross follows the sign of the MACD spread: MACD above ",
    "the signal line targets long exposure, while MACD below the signal line targets short exposure. ",
    "MACD contrarian uses the named strategyr inverse rule: positive spread targets short exposure, and ",
    "negative spread targets long exposure. The real-data gallery uses 10/35/12 for MACD cross and ",
    "10/35/7 for MACD contrarian; this simulated board keeps those parameter choices but removes asset-specific noise.\n\n"
  )
)

board <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_price, p_cross, p_contrarian, p_target),
  n_rows = 4,
  n_cols = 1,
  title = "How MACD Cross and MACD Contrarian Translate Signals Into Exposure",
  bottom = paste(
    "Simulated OHLC data. MACD spread is MACD line minus signal line.",
    "MACD cross follows the spread sign; MACD contrarian flips it."
  ),
  style = style,
  context = context
)

print(board)
