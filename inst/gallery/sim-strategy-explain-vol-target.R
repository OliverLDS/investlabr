library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(52)
style <- "strategy_explain"
context <- "report"
target_size <- 0.95
vol_target <- 0.2
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

n_obs <- 180L
ret <- c(stats::rnorm(70, 0.001, 0.006), stats::rnorm(45, -0.0005, 0.025), stats::rnorm(65, 0.001, 0.010))
close <- 100 * exp(cumsum(ret))
datetime <- as.POSIXct(as.Date("2026-01-01") + seq_along(close) - 1L, tz = "UTC")
DT <- data.table(datetime, close)
target_vec <- strategyr::strat_vol_target_tgt_pos(DT, trend_n = 20L, rv_n = 20L, vol_target = vol_target, max_leverage = target_size)
data.table::set(DT, j = "target", value = target_vec)
DT[, `:=`(ema_20 = get("ema_20"), rv_20 = get("rv_20"))]

p_vol <- ggplot(DT, aes(datetime, rv_20)) +
  geom_hline(yintercept = vol_target, linetype = "dashed", color = resolved$muted) +
  geom_line(color = palette[1], linewidth = 0.9, na.rm = TRUE) +
  labs(title = "Realized volatility controls position size", subtitle = "Exposure scales as target volatility divided by realized volatility, capped at max leverage.", x = NULL, y = "Annualized realized vol")
p_vol <- investlabr::viz_theme_apply(p_vol, style = style, context = context, show_compiler = FALSE)

p_trend <- ggplot(DT, aes(datetime)) +
  geom_line(aes(y = close, color = "Close"), linewidth = 0.8) +
  geom_line(aes(y = ema_20, color = "EMA20"), linewidth = 0.8, na.rm = TRUE) +
  scale_color_manual(values = c("Close" = resolved$ink, "EMA20" = palette[1])) +
  labs(title = "Trend filter controls direction", subtitle = "Price above EMA20 gives positive direction; price below EMA20 gives negative direction.", x = NULL, y = "Simulated price", color = NULL)
p_trend <- investlabr::viz_theme_apply(p_trend, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

p_target <- ggplot(DT, aes(datetime, target)) + geom_hline(yintercept = 0, linetype = "dotted", color = resolved$muted) + geom_line(color = palette[2], linewidth = 0.9, na.rm = TRUE) + scale_y_continuous(limits = c(-1, 1)) + labs(title = "Target exposure", subtitle = "Position size falls when realized volatility rises, even if trend direction remains unchanged.", x = NULL, y = "Target")
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, show_compiler = FALSE)

cat("This simulated strategy-explanation example mirrors real-data-strategyr-vol-target-backtest.R. Direction comes from price versus EMA20, while exposure size scales with the ratio of 20% target volatility to 20-day realized volatility and is capped at 0.95.\n\n")

print(investlabr::gen_grid_of_plots_with_labels(list(p_trend, p_vol, p_target), 3, 1, title = "How Volatility Targeting Separates Direction From Size", bottom = "Simulated close data. Trend window = 20, realized-volatility window = 20, target volatility = 20%, max leverage = 0.95.", style = style, context = context))
