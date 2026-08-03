library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

style <- "strategy_explain"
context <- "report"
target_size <- 0.95
long_threshold <- 0.006
short_threshold <- 0.004
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

n_obs <- 150L
datetime <- as.POSIXct(as.Date("2026-01-01") + seq_len(n_obs) - 1L, tz = "UTC")
short_rate <- 0.042 + 0.001 * sin(seq_len(n_obs) / 12)
slope <- c(seq(0.003, 0.007, length.out = 50), seq(0.007, 0.0035, length.out = 50), seq(0.0035, 0.0065, length.out = 50))
long_rate <- short_rate + slope
DT <- data.table(datetime, short_rate, long_rate)
direct_target <- strategyr::strat_curve_steepener_tgt_pos(DT, long_threshold = long_threshold, short_threshold = short_threshold, target_size = target_size)
data.table::set(DT, j = "direct_target", value = direct_target)
DT[, contrarian_target := -direct_target]

p_slope <- ggplot(DT, aes(datetime, curve_slope * 10000)) +
  geom_hline(yintercept = c(short_threshold, long_threshold) * 10000, linetype = "dashed", color = resolved$muted) +
  geom_line(color = palette[1], linewidth = 0.9) +
  labs(title = "Treasury 10Y-2Y slope signal", subtitle = "Above 60bp targets steepener exposure; below 40bp targets flattener exposure; inside the zone is flat.", x = NULL, y = "Basis points")
p_slope <- investlabr::viz_theme_apply(p_slope, style = style, context = context, show_compiler = FALSE)

target_long <- data.table::melt(DT[, .(datetime, `Direct steepener` = direct_target, `Contrarian inverse` = contrarian_target)], id.vars = "datetime", variable.name = "strategy", value.name = "target")
p_target <- ggplot(target_long, aes(datetime, target, color = strategy)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = resolved$muted) +
  geom_step(linewidth = 0.9) +
  scale_color_manual(values = c("Direct steepener" = palette[1], "Contrarian inverse" = palette[2])) +
  scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) +
  labs(title = "Target exposure", subtitle = "The real-data example also evaluates the contrarian inverse of the direct curve-steepener signal.", x = NULL, y = NULL, color = NULL)
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

cat("This simulated strategy-explanation example mirrors real-data-strategyr-curve-steepener-backtest.R. The direct rule takes long steepener exposure when the lagged curve slope is above 60bp, short exposure when below 40bp, and flat inside the neutral zone; the contrarian strategy flips those positions.\n\n")

print(investlabr::gen_grid_of_plots_with_labels(list(p_slope, p_target), 2, 1, title = "How Curve Steepener Signals Convert Slope Zones Into Exposure", bottom = "Simulated Treasury slope data. Long threshold = 60bp and short threshold = 40bp.", style = style, context = context))
