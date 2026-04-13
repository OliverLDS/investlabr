library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(53)
style <- "strategy_explain"
context <- "report"
target_size <- 0.95
fast <- 20L
slow <- 50L
low_atr_threshold <- 50L
freshness_floor <- 250L
tp_ratio <- 0.20
sl_ratio <- 0.10
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

close <- c(seq(100, 94, length.out = 55), seq(94, 119, length.out = 70), seq(119, 101, length.out = 55)) + stats::rnorm(180, 0, 0.45)
datetime <- as.POSIXct(as.Date("2026-01-01") + seq_along(close) - 1L, tz = "UTC")
open <- data.table::shift(close, fill = close[1])
high <- pmax(open, close) + 0.5
low <- pmin(open, close) - 0.5
DT <- data.table(datetime, open, high, low, close)
strategyr::calc_EMA(DT, ns = c(fast, slow))
DT[, low_atr_50 := TRUE]
target_vec <- target_size * strategyr::strat_ema_cross_tgt_pos(
  DT,
  fast = fast,
  slow = slow,
  low_atr_threshold = low_atr_threshold,
  freshness_floor = freshness_floor,
  tp_ratio = tp_ratio,
  sl_ratio = sl_ratio,
  compute_features = FALSE
)
data.table::set(DT, j = "target", value = target_vec)

p_price <- investlabr::viz_candle_base(DT, style = style, context = context, show_compiler = FALSE) +
  geom_line(aes(y = ema_20, color = "EMA20 fast"), linewidth = 0.8, na.rm = TRUE) +
  geom_line(aes(y = ema_50, color = "EMA50 slow"), linewidth = 0.8, na.rm = TRUE) +
  scale_color_manual(values = c("EMA20 fast" = palette[1], "EMA50 slow" = palette[2])) +
  labs(title = "EMA cross signal with ATR gate", subtitle = "The real-data example uses fast = 20, slow = 50, low-ATR percentile = 50, and loose TP/SL guards.", x = NULL, y = "Simulated price", color = NULL)
p_price <- investlabr::viz_theme_apply(p_price, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

p_target <- ggplot(DT, aes(datetime, target)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = resolved$muted) +
  geom_step(color = palette[1], linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) +
  labs(title = "Target exposure", subtitle = "Fresh EMA cross direction creates target exposure only while the low-ATR and guard conditions are satisfied.", x = NULL, y = NULL)
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, show_compiler = FALSE)

cat("This simulated strategy-explanation example mirrors real-data-strategyr-ema-cross-backtest.R. The strategy uses fast versus slow EMA direction, a low-ATR gate, cross freshness, and take-profit/stop-loss guards. In this simplified simulated example the low-ATR gate is held open so the EMA-cross mechanics are easier to see.\n\n")

print(investlabr::gen_grid_of_plots_with_labels(list(p_price, p_target), 2, 1, title = "How EMA Cross Converts Trend Crosses Into Exposure", bottom = "Simulated OHLC data. EMA fast/slow = 20/50; low-ATR gate is held TRUE for explanation.", style = style, context = context))
