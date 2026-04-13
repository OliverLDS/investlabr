library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

style <- "strategy_explain"
context <- "report"
target_size <- 0.95
lower <- 7L
upper <- 13L
cycle_N <- 180L
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

n_obs <- 140L
datetime <- as.POSIXct(as.Date("2026-01-01") + seq_len(n_obs) - 1L, tz = "UTC")
ladder_index <- round(c(seq(9, 6, length.out = 35), seq(6, 14, length.out = 40), seq(14, 8, length.out = 35), seq(8, 12, length.out = 30)))
close <- 100 + cumsum((ladder_index - 9) / 8 + stats::rnorm(n_obs, 0, 0.4))
open <- data.table::shift(close, fill = close[1]) + stats::rnorm(n_obs, 0, 0.2)
high <- pmax(open, close) + 0.5
low <- pmin(open, close) - 0.5
DT <- data.table(datetime, open, high, low, close)
DT[, ladder_index_180 := ladder_index]
bounce_target <- strategyr::strat_ladder_bounce_tgt_pos(DT, lower = lower, upper = upper, target_size = target_size, compute_ladder = FALSE)
breakout_target <- strategyr::strat_ladder_breakout_tgt_pos(DT, lower = lower, upper = upper, target_size = target_size, compute_ladder = FALSE)
data.table::set(DT, j = "bounce_target", value = bounce_target)
data.table::set(DT, j = "breakout_target", value = breakout_target)

p_ladder <- ggplot(DT, aes(datetime, ladder_index_180)) +
  geom_hline(yintercept = c(lower, upper), linetype = "dashed", color = resolved$muted) +
  geom_hline(yintercept = 9, linetype = "dotted", color = resolved$muted) +
  geom_line(color = palette[1], linewidth = 0.9) +
  labs(title = "Fibonacci ladder index zones", subtitle = "Lower/upper outer zones are interpreted differently by bounce and breakout rules.", x = NULL, y = "Ladder index")
p_ladder <- investlabr::viz_theme_apply(p_ladder, style = style, context = context, show_compiler = FALSE)

target_long <- data.table::melt(DT[, .(datetime, `Ladder bounce` = bounce_target, `Ladder breakout` = breakout_target)], id.vars = "datetime", variable.name = "strategy", value.name = "target")
p_target <- ggplot(target_long, aes(datetime, target, color = strategy)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = resolved$muted) +
  geom_step(linewidth = 0.9) +
  scale_color_manual(values = c("Ladder bounce" = palette[1], "Ladder breakout" = palette[2])) +
  scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) +
  labs(title = "Bounce versus breakout interpretation", subtitle = "Bounce fades outer ladder zones; breakout follows them as continuation signals.", x = NULL, y = NULL, color = NULL)
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, legend_position = "bottom", show_compiler = FALSE)

cat("This simulated strategy-explanation example mirrors the ladder-bounce and ladder-breakout real-data scripts. The same ladder-index extremes are used with opposite interpretation: bounce treats outer zones as mean-reversion opportunities, while breakout treats them as continuation signals.\n\n")

print(investlabr::gen_grid_of_plots_with_labels(list(p_ladder, p_target), 2, 1, title = "How Ladder Bounce and Ladder Breakout Interpret the Same Zones", bottom = "Simulated ladder index. Lower = 7 and upper = 13 match the real-data gallery examples.", style = style, context = context))
