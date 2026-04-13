# Purpose: Explain the Donchian breakout rule used by the real-data Donchian backtest.
# Data source: Simulated OHLC data.
# Main functions demonstrated: strategyr::strat_donchian_breakout_tgt_pos(), investlabr visualization styling.
# Promotion candidate: Strategy explanation panels may later become investlabr viz helpers backed by strategyr diagnostics.

library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(42)

style <- "strategy_explain"
context <- "report"
donchian_n <- 20L
target_size <- 0.95
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

# The simulated path is intentionally stylized: range, upside breakout, pullback,
# downside breakout, then recovery. This makes the rule mechanics visible.
close <- c(
  100 + sin(seq_len(30) / 2.4) * 1.4 + stats::rnorm(30, 0, 0.25),
  seq(101, 118, length.out = 25) + stats::rnorm(25, 0, 0.35),
  seq(117, 105, length.out = 25) + stats::rnorm(25, 0, 0.35),
  seq(104, 86, length.out = 25) + stats::rnorm(25, 0, 0.35),
  seq(87, 101, length.out = 25) + stats::rnorm(25, 0, 0.35)
)

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

tgt_pos <- strategyr::strat_donchian_breakout_tgt_pos(
  DT,
  n = donchian_n,
  target_size = target_size,
  compute_features = TRUE
)

upper_col <- paste0("dc_high_", donchian_n)
lower_col <- paste0("dc_low_", donchian_n)
DT[, `:=`(
  prior_upper = data.table::shift(.SD[[upper_col]], n = 1L),
  prior_lower = data.table::shift(.SD[[lower_col]], n = 1L),
  target = tgt_pos
)]

DT[, signal := data.table::fifelse(
  close > prior_upper,
  "Long breakout",
  data.table::fifelse(close < prior_lower, "Short breakout", NA_character_)
)]

signal_dt <- DT[!is.na(signal)]
signal_dt <- signal_dt[
  target != data.table::shift(target, n = 1L, fill = 0) |
    seq_len(.N) == 1L
]

regime_dt <- DT[
  ,
  .(
    start = min(datetime),
    end = max(datetime),
    target = target[1]
  ),
  by = .(regime = data.table::rleid(target))
][target != 0]
regime_dt[, regime_label := data.table::fifelse(target > 0, "Long target", "Short target")]

p_price <- investlabr::viz_candle_base(
  DT,
  style = style,
  context = context,
  show_compiler = FALSE
) +
  geom_rect(
    data = regime_dt,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = regime_label
    ),
    inherit.aes = FALSE,
    alpha = 0.06
  ) +
  geom_line(aes(y = prior_upper, color = "Prior upper channel"), linewidth = 0.75, na.rm = TRUE) +
  geom_line(aes(y = prior_lower, color = "Prior lower channel"), linewidth = 0.75, na.rm = TRUE) +
  geom_point(
    data = signal_dt,
    aes(y = close, shape = signal),
    size = 2.6,
    stroke = 1.0,
    color = resolved$accent
  ) +
  geom_vline(
    data = signal_dt,
    aes(xintercept = datetime),
    linetype = "dashed",
    linewidth = 0.45,
    color = resolved$accent,
    alpha = 0.75
  ) +
  ggrepel::geom_label_repel(
    data = signal_dt,
    aes(y = close, label = signal),
    size = 3.1,
    label.padding = grid::unit(0.12, "lines"),
    min.segment.length = 0,
    seed = 42,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Prior upper channel" = palette[1],
      "Prior lower channel" = palette[2]
    )
  ) +
  scale_fill_manual(
    values = c(
      "Long target" = palette[1],
      "Short target" = palette[2]
    )
  ) +
  scale_shape_manual(values = c("Long breakout" = 24, "Short breakout" = 25)) +
  labs(
    title = "Donchian breakout rule on a simulated price path",
    subtitle = sprintf(
      "Close above the prior %s-bar high targets long exposure; close below the prior %s-bar low targets short exposure.",
      donchian_n,
      donchian_n
    ),
    x = NULL,
    y = "Simulated price",
    color = NULL,
    fill = NULL,
    shape = NULL
  )
p_price <- investlabr::viz_theme_apply(
  p_price,
  style = style,
  context = context,
  legend_position = "bottom",
  show_compiler = FALSE
)

pos_dt <- data.table::copy(DT)
pos_dt[, target_label := data.table::fifelse(
  target > 0,
  "Long target",
  data.table::fifelse(target < 0, "Short target", "Flat")
)]

p_position <- ggplot(pos_dt, aes(x = datetime, y = target)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dotted", color = resolved$muted) +
  geom_step(aes(color = target_label), linewidth = 0.9, na.rm = TRUE) +
  geom_point(
    data = signal_dt,
    aes(x = datetime, y = target),
    color = resolved$accent,
    size = 2.2,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Long target" = palette[1],
      "Short target" = palette[2],
      "Flat" = resolved$muted
    )
  ) +
  scale_y_continuous(
    breaks = c(-target_size, 0, target_size),
    labels = c("Short", "Flat", "Long"),
    limits = c(-1.05, 1.05)
  ) +
  labs(
    title = "Target exposure is stateful",
    subtitle = "After a breakout, the target stays in force until an opposite channel breach occurs.",
    x = NULL,
    y = NULL,
    color = NULL
  )
p_position <- investlabr::viz_theme_apply(
  p_position,
  style = style,
  context = context,
  legend_position = "bottom",
  show_compiler = FALSE
)

cat(
  paste0(
    "This simulated strategy-explanation example mirrors the Donchian breakout logic used in ",
    "real-data-strategyr-donchian-backtest.R. The channel is based on the prior ",
    donchian_n,
    " bars, not the current bar. A close above the prior upper channel changes the target to long; ",
    "a close below the prior lower channel changes the target to short; otherwise the previous target is carried forward. ",
    "The example is deliberately stylized so the two breakout transitions and the stateful target exposure are easy to see.\n\n"
  )
)

board <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_price, p_position),
  n_rows = 2,
  n_cols = 1,
  title = "How a Donchian Breakout Strategy Changes Position",
  bottom = paste(
    "Simulated OHLC data. The dashed vertical lines mark first observed long/short breakout transitions.",
    "The channel lines use prior-window values, matching the look-ahead-safe rule used by strategyr."
  ),
  style = style,
  context = context
)

print(board)
