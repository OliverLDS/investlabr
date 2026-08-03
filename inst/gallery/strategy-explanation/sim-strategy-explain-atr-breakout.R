library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(48)
style <- "strategy_explain"
context <- "report"
n <- 14L
atr_mult <- 1
target_size <- 0.95
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

close <- c(seq(100, 105, length.out = 45), seq(105, 120, length.out = 15), seq(119, 107, length.out = 35), seq(107, 91, length.out = 18), seq(92, 105, length.out = 37)) + stats::rnorm(150, 0, 0.35)
datetime <- as.POSIXct(as.Date("2026-01-01") + seq_along(close) - 1L, tz = "UTC")
open <- data.table::shift(close, fill = close[1]) + stats::rnorm(length(close), 0, 0.25)
high <- pmax(open, close) + stats::runif(length(close), 0.2, 0.9)
low <- pmin(open, close) - stats::runif(length(close), 0.2, 0.9)
DT <- data.table(datetime, open, high, low, close)
target_vec <- strategyr::strat_atr_breakout_tgt_pos(DT, n = n, atr_mult = atr_mult, target_size = target_size, compute_features = TRUE)
data.table::set(DT, j = "target", value = target_vec)
DT[, `:=`(prev_close = data.table::shift(close), prev_atr = data.table::shift(get(paste0("atr_", n))))]
DT[, `:=`(upper_trigger = prev_close + atr_mult * prev_atr, lower_trigger = prev_close - atr_mult * prev_atr)]
data.table::set(DT, j = "prev_target", value = data.table::shift(DT[["target"]], n = 1L, fill = 0))
events <- DT[target != prev_target & is.finite(prev_atr)]
events[, signal := data.table::fifelse(target > 0, "Upside ATR breakout", "Downside ATR breakout")]

p_price <- investlabr::viz_candle_base(DT, style = style, context = context, show_compiler = FALSE) +
  geom_line(aes(y = upper_trigger, color = "Prior close + ATR"), linewidth = 0.7, na.rm = TRUE) +
  geom_line(aes(y = lower_trigger, color = "Prior close - ATR"), linewidth = 0.7, na.rm = TRUE) +
  geom_vline(data = events, aes(xintercept = datetime), linetype = "dashed", color = resolved$accent, linewidth = 0.35, alpha = 0.5) +
  ggrepel::geom_label_repel(data = events, aes(y = close, label = signal), size = 3, seed = 48, min.segment.length = 0, show.legend = FALSE) +
  scale_color_manual(values = c("Prior close + ATR" = palette[1], "Prior close - ATR" = palette[2])) +
  labs(title = "ATR breakout rule", subtitle = "A close beyond the prior close plus/minus prior ATR changes the target direction.", x = NULL, y = "Simulated price", color = NULL)
p_price <- investlabr::viz_theme_apply(p_price, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

p_target <- ggplot(DT, aes(datetime, target)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = resolved$muted) +
  geom_step(color = palette[1], linewidth = 0.9) +
  scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) +
  labs(title = "Target exposure", subtitle = "The target stays in the breakout direction until an opposite ATR breakout occurs.", x = NULL, y = NULL)
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, show_compiler = FALSE)

cat("This simulated strategy-explanation example mirrors real-data-strategyr-atr-breakout-backtest.R. The rule targets long exposure after an upside close-to-close move larger than one prior ATR and short exposure after a downside move larger than one prior ATR.\n\n")

print(investlabr::gen_grid_of_plots_with_labels(list(p_price, p_target), 2, 1, title = "How ATR Breakout Converts Large Moves Into Exposure", bottom = "Simulated OHLC data. ATR window = 14 and ATR multiple = 1, matching the real-data gallery example.", style = style, context = context))
