library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(49)
style <- "strategy_explain"
context <- "report"
z_n <- 20L
entry_z <- 2
exit_z <- 0.5
target_size <- 0.95
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

n_obs <- 170L
datetime <- as.POSIXct(as.Date("2026-01-01") + seq_len(n_obs) - 1L, tz = "UTC")
benchmark_close <- 100 + cumsum(stats::rnorm(n_obs, 0.04, 0.45))
spread_shock <- 1.8 * sin(seq_len(n_obs) / 9) + c(rep(0, 55), seq(0, 7, length.out = 20), seq(7, -6, length.out = 45), seq(-6, 0, length.out = 50))
close <- benchmark_close + spread_shock
open <- data.table::shift(close, fill = close[1])
high <- pmax(open, close) + 0.4
low <- pmin(open, close) - 0.4
DT <- data.table(datetime, open, high, low, close, benchmark_close)
spread_target <- strategyr::strat_pair_spread_revert_tgt_pos(DT, z_n = z_n, entry_z = entry_z, exit_z = exit_z, target_size = target_size)
ratio_target <- strategyr::strat_ratio_revert_tgt_pos(DT, z_n = z_n, entry_z = entry_z, exit_z = exit_z, target_size = target_size)
data.table::set(DT, j = "spread_target", value = spread_target)
data.table::set(DT, j = "ratio_target", value = ratio_target)
spread_z <- grep("^zscore_spread", names(DT), value = TRUE)[1]
ratio_z <- grep("^zscore_ratio", names(DT), value = TRUE)[1]
DT[, `:=`(spread_z = get(spread_z), ratio_z = get(ratio_z))]

z_long <- data.table::melt(DT[, .(datetime, `Pair spread z-score` = spread_z, `Price ratio z-score` = ratio_z)], id.vars = "datetime", variable.name = "measure", value.name = "z")
p_z <- ggplot(z_long, aes(datetime, z, color = measure)) +
  geom_hline(yintercept = c(-entry_z, entry_z), linetype = "dashed", color = resolved$muted) +
  geom_hline(yintercept = c(-exit_z, exit_z), linetype = "dotted", color = resolved$muted) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  scale_color_manual(values = c("Pair spread z-score" = palette[1], "Price ratio z-score" = palette[2])) +
  labs(title = "Pair-spread and ratio z-score signals", subtitle = "Extreme positive z-scores target short exposure; extreme negative z-scores target long exposure.", x = NULL, y = "Rolling z-score", color = NULL)
p_z <- investlabr::viz_theme_apply(p_z, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

target_long <- data.table::melt(DT[, .(datetime, `Pair spread reversion` = spread_target, `Ratio reversion` = ratio_target)], id.vars = "datetime", variable.name = "strategy", value.name = "target")
p_target <- ggplot(target_long, aes(datetime, target, color = strategy)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = resolved$muted) +
  geom_step(linewidth = 0.9, na.rm = TRUE) +
  scale_color_manual(values = c("Pair spread reversion" = palette[1], "Ratio reversion" = palette[2])) +
  scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) +
  labs(title = "Target exposure", subtitle = "Both strategies use entry z = 2 and exit z = 0.5; they differ in whether the signal is a spread or ratio.", x = NULL, y = NULL, color = NULL)
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

cat("This simulated strategy-explanation example mirrors the pair-spread and ratio reversion real-data scripts. Both standardize a relationship to a benchmark and trade mean reversion: high z-score targets short exposure, low z-score targets long exposure, and small z-score exits toward flat.\n\n")

print(investlabr::gen_grid_of_plots_with_labels(list(p_z, p_target), 2, 1, title = "How Pair-Spread and Ratio Reversion Use Z-Score Extremes", bottom = "Simulated pair data. Entry z = 2 and exit z = 0.5 match the real-data gallery examples.", style = style, context = context))
