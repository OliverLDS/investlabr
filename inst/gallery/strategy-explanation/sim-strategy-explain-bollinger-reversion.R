# Purpose: Explain the Bollinger reversion rule used by the real-data Bollinger backtest.
# Data source: Simulated OHLC data.
# Main functions demonstrated: strategyr::strat_bollinger_revert_tgt_pos(), investlabr visualization styling.
# Promotion candidate: Strategy explanation panels may later become investlabr viz helpers backed by strategyr diagnostics.

library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(47)

style <- "strategy_explain"
context <- "report"
target_size <- 0.95
bollinger_n <- 15L
bollinger_k <- 3.0
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

n_obs <- 180L
base <- 100 + 2.2 * sin(seq_len(n_obs) / 7) + stats::rnorm(n_obs, 0, 0.8)
shock <- rep(0, n_obs)
shock[45:52] <- seq(0, -13, length.out = 8)
shock[53:72] <- seq(-11, 2, length.out = 20)
shock[105:112] <- seq(0, 15, length.out = 8)
shock[113:135] <- seq(13, -1, length.out = 23)
close <- base + shock

datetime <- as.POSIXct(as.Date("2026-01-01") + seq_along(close) - 1L, tz = "UTC")
open <- data.table::shift(close, n = 1L, type = "lag", fill = close[1]) +
  stats::rnorm(length(close), 0, 0.35)
high <- pmax(open, close) + stats::runif(length(close), 0.2, 0.9)
low <- pmin(open, close) - stats::runif(length(close), 0.2, 0.9)

DT <- data.table::data.table(datetime, open, high, low, close)
target <- strategyr::strat_bollinger_revert_tgt_pos(
  DT,
  n = bollinger_n,
  k = bollinger_k,
  target_size = target_size,
  compute_features = TRUE
)

k_tag <- "3"
DT[, `:=`(
  bb_mid = .SD[[paste0("bb_mid_", bollinger_n)]],
  bb_high = .SD[[paste0("bb_high_", bollinger_n, "_", k_tag)]],
  bb_low = .SD[[paste0("bb_low_", bollinger_n, "_", k_tag)]],
  target = target
)]

DT[, prev_target := data.table::shift(target, n = 1L, fill = 0)]
events <- DT[target != prev_target]
events[, signal := data.table::fifelse(
  target > 0,
  "Enter long: lower band touch",
  data.table::fifelse(target < 0, "Enter short: upper band touch", "Exit: returned to mid band")
)]

p_price <- investlabr::viz_candle_base(DT, style = style, context = context, show_compiler = FALSE) +
  geom_line(aes(y = bb_high, color = "Upper band"), linewidth = 0.75, na.rm = TRUE) +
  geom_line(aes(y = bb_mid, color = "Mid band"), linewidth = 0.65, linetype = "dotted", na.rm = TRUE) +
  geom_line(aes(y = bb_low, color = "Lower band"), linewidth = 0.75, na.rm = TRUE) +
  geom_vline(data = events, aes(xintercept = datetime), linetype = "dashed", linewidth = 0.35, color = resolved$accent, alpha = 0.45) +
  geom_point(data = events, aes(y = close), color = resolved$accent, size = 2.2, na.rm = TRUE) +
  ggrepel::geom_label_repel(data = events, aes(y = close, label = signal), size = 3, min.segment.length = 0, seed = 47, show.legend = FALSE, na.rm = TRUE) +
  scale_color_manual(values = c("Upper band" = palette[4], "Mid band" = resolved$muted, "Lower band" = palette[3])) +
  labs(
    title = "Bollinger reversion on a simulated price path",
    subtitle = sprintf("The real-data example uses n = %s and k = %.1f: upper/lower bands sit k standard deviations around the rolling mean.", bollinger_n, bollinger_k),
    x = NULL,
    y = "Simulated price",
    color = NULL
  )
p_price <- investlabr::viz_theme_apply(p_price, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

p_target <- ggplot(DT, aes(x = datetime, y = target)) +
  geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dotted", color = resolved$muted) +
  geom_step(color = palette[1], linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) +
  labs(
    title = "Target exposure",
    subtitle = "Lower-band touches target long, upper-band touches target short, and a return to the mid band exits.",
    x = NULL,
    y = NULL
  )
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, show_compiler = FALSE)

cat(
  paste0(
    "This simulated strategy-explanation example mirrors real-data-strategyr-bollinger-backtest.R. ",
    "The Bollinger rule defines a rolling mid band and upper/lower bands at ", bollinger_k, " rolling standard deviations. ",
    "A lower-band touch opens a long target, an upper-band touch opens a short target, and a return to the mid band flattens the position.\n\n"
  )
)

board <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_price, p_target),
  n_rows = 2,
  n_cols = 1,
  title = "How Bollinger Reversion Converts Band Touches Into Exposure",
  bottom = "Simulated OHLC data. The band parameters match the real-data Bollinger gallery example.",
  style = style,
  context = context
)

print(board)
