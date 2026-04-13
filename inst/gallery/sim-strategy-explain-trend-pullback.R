library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(51)
style <- "strategy_explain"
context <- "report"
target_size <- 0.95
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

close <- c(seq(100, 118, length.out = 55), seq(118, 110, length.out = 25), seq(110, 122, length.out = 35), seq(122, 100, length.out = 45), seq(100, 108, length.out = 30)) + stats::rnorm(190, 0, 0.55)
datetime <- as.POSIXct(as.Date("2026-01-01") + seq_along(close) - 1L, tz = "UTC")
open <- data.table::shift(close, fill = close[1])
high <- pmax(open, close) + 0.5
low <- pmin(open, close) - 0.5
DT <- data.table(datetime, open, high, low, close)
target_vec <- strategyr::strat_trend_pullback_tgt_pos(DT, trend_n = 20L, rsi_n = 14L, pullback_long = 55, pullback_short = 45, exit_rsi = 50, target_size = target_size)
data.table::set(DT, j = "target", value = target_vec)
DT[, `:=`(ema_20 = get("ema_20"), rsi_14 = get("rsi_14"))]

p_price <- investlabr::viz_candle_base(DT, style = style, context = context, show_compiler = FALSE) +
  geom_line(aes(y = ema_20, color = "EMA20 trend filter"), linewidth = 0.8, na.rm = TRUE) +
  scale_color_manual(values = c("EMA20 trend filter" = palette[1])) +
  labs(title = "Trend filter", subtitle = "Long pullbacks require price above EMA20; short pullbacks require price below EMA20.", x = NULL, y = "Simulated price", color = NULL)
p_price <- investlabr::viz_theme_apply(p_price, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

p_rsi <- ggplot(DT, aes(datetime, rsi_14)) +
  geom_hline(yintercept = c(55, 45, 50), linetype = c("dashed", "dashed", "dotted"), color = resolved$muted) +
  geom_line(color = palette[2], linewidth = 0.9, na.rm = TRUE) +
  labs(title = "RSI pullback trigger", subtitle = "In an uptrend RSI <= 55 can enter long; in a downtrend RSI >= 45 can enter short; RSI 50 exits.", x = NULL, y = "RSI")
p_rsi <- investlabr::viz_theme_apply(p_rsi, style = style, context = context, show_compiler = FALSE)

p_target <- ggplot(DT, aes(datetime, target)) + geom_hline(yintercept = 0, linetype = "dotted", color = resolved$muted) + geom_step(color = palette[1], linewidth = 0.9, na.rm = TRUE) + scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) + labs(title = "Target exposure", subtitle = "Trend and pullback conditions must agree before exposure is opened.", x = NULL, y = NULL)
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, show_compiler = FALSE)

cat("This simulated strategy-explanation example mirrors real-data-strategyr-trend-pullback-backtest.R. The rule combines an EMA20 trend filter with RSI pullback thresholds: pullbacks in an uptrend can enter long, pullbacks in a downtrend can enter short, and RSI or trend reversal exits.\n\n")

print(investlabr::gen_grid_of_plots_with_labels(list(p_price, p_rsi, p_target), 3, 1, title = "How Trend Pullback Combines Trend Direction With RSI Pullbacks", bottom = "Simulated OHLC data. EMA = 20, RSI = 14, pullback thresholds = 55/45, and exit RSI = 50.", style = style, context = context))
