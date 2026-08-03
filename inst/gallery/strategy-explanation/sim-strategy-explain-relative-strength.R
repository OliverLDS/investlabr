library(data.table)
library(ggplot2)
library(investlabr)
library(strategyr)

set.seed(50)
style <- "strategy_explain"
context <- "report"
n <- 20L
long_threshold <- 1.02
short_threshold <- 0.98
target_size <- 0.95
resolved <- investlabr::viz_style_get(style, context)
palette <- investlabr::viz_palette_get(style, context, "discrete")

n_obs <- 160L
datetime <- as.POSIXct(as.Date("2026-01-01") + seq_len(n_obs) - 1L, tz = "UTC")
benchmark_close <- 100 * exp(cumsum(stats::rnorm(n_obs, 0.0003, 0.006)))
rel <- c(seq(1, 1.05, length.out = 50), seq(1.05, 0.94, length.out = 55), seq(0.94, 1.04, length.out = 55))
close <- benchmark_close * rel
DT <- data.table(datetime, close, benchmark_close)
target_vec <- strategyr::strat_relative_strength_tgt_pos(DT, n = n, long_threshold = long_threshold, short_threshold = short_threshold, target_size = target_size)
data.table::set(DT, j = "target", value = target_vec)
rs_col <- paste0("relative_strength_", n)
DT[, rs := get(rs_col)]

p_rs <- ggplot(DT, aes(datetime, rs)) +
  geom_hline(yintercept = c(long_threshold, short_threshold), linetype = "dashed", color = resolved$muted) +
  geom_hline(yintercept = 1, linetype = "dotted", color = resolved$muted) +
  geom_line(color = palette[1], linewidth = 0.9, na.rm = TRUE) +
  labs(title = "Rolling relative strength", subtitle = "Strong traded-asset performance versus benchmark targets long; weak relative performance targets short.", x = NULL, y = "Relative strength")
p_rs <- investlabr::viz_theme_apply(p_rs, style = style, context = context, show_compiler = FALSE)

p_target <- ggplot(DT, aes(datetime, target)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = resolved$muted) +
  geom_step(color = palette[1], linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(breaks = c(-target_size, 0, target_size), labels = c("Short", "Flat", "Long"), limits = c(-1.05, 1.05)) +
  labs(title = "Target exposure", subtitle = "Thresholds 1.02/0.98 match the real-data relative-strength gallery example.", x = NULL, y = NULL)
p_target <- investlabr::viz_theme_apply(p_target, style = style, context = context, show_compiler = FALSE)

cat("This simulated strategy-explanation example mirrors real-data-strategyr-relative-strength-backtest.R. The rule compares rolling performance of the traded asset with a benchmark: sufficiently strong relative performance targets long exposure and sufficiently weak relative performance targets short exposure.\n\n")

print(investlabr::gen_grid_of_plots_with_labels(list(p_rs, p_target), 2, 1, title = "How Relative Strength Converts Benchmark Comparison Into Exposure", bottom = "Simulated traded and benchmark series. Relative-strength window = 20 and thresholds = 1.02/0.98.", style = style, context = context))
