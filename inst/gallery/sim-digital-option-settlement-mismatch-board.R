library(data.table)
library(ggplot2)
library(investlabr)

set.seed(1)

# Stylized user inputs
p_yes_up <- 0.34
p_yes_down <- 0.52
pm_upper <- 3200
pm_lower <- 3100

K <- 3150
prem_call <- 120
prem_put <- 110
contracts <- 1
fee_trade <- 0
fee_exercise <- 0

S_min <- 2400
S_max <- 3400
n_grid <- 2001L

use_two_date_model <- TRUE
mu_annual <- 0
sigma_annual <- 0.80
days_between <- 7L
n_mc <- 20000L
quantile_q <- 0.01

pm_cost <- p_yes_up + p_yes_down

pm_payoff <- function(S7) {
  payout <- as.numeric(S7 > pm_upper | S7 < pm_lower)
  payout - pm_cost
}

opt_payoff <- function(ST) {
  intrinsic <- pmax(ST - K, 0) + pmax(K - ST, 0)
  contracts * intrinsic - contracts * (prem_call + prem_put) - fee_trade - fee_exercise
}

combined_payoff <- function(S7, ST) {
  300 * pm_payoff(S7) + opt_payoff(ST)
}

grid <- data.table(S = seq(S_min, S_max, length.out = n_grid))
grid[, `:=`(
  digital = 300 * pm_payoff(S),
  option = opt_payoff(S)
)]
grid[, combo_same_time := digital + option]
grid[, `:=`(
  valley_option = pmin(option, 0),
  valley_combo = pmin(combo_same_time, 0)
)]

S7_vals <- seq(S_min, S_max, length.out = 241L)
ST_vals <- seq(S_min, S_max, length.out = 241L)

heat <- CJ(S7 = S7_vals, ST = ST_vals)
heat[, payoff := combined_payoff(S7, ST)]

simulate_conditional <- function(S7_vec, mu_annual, sigma_annual, days_between, n_mc = 20000L, q = 0.01) {
  T_years <- days_between / 365
  out <- vector("list", length(S7_vec))

  for (i in seq_along(S7_vec)) {
    S7 <- S7_vec[i]
    Z <- rnorm(n_mc)
    ST <- S7 * exp((mu_annual - 0.5 * sigma_annual^2) * T_years + sigma_annual * sqrt(T_years) * Z)
    pay <- combined_payoff(S7, ST)

    out[[i]] <- data.table(
      S7 = S7,
      payoff_mean = mean(pay),
      payoff_median = median(pay),
      payoff_q = as.numeric(stats::quantile(pay, probs = q))
    )
  }

  rbindlist(out)
}

risk_dt <- if (use_two_date_model) {
  simulate_conditional(
    S7_vec = S7_vals,
    mu_annual = mu_annual,
    sigma_annual = sigma_annual,
    days_between = days_between,
    n_mc = n_mc,
    q = quantile_q
  )
} else NULL

style_name <- "institutional_blue"
context_name <- "report"

p1 <- ggplot(grid, aes(S)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(aes(y = digital, color = "Digital leg"), linewidth = 0.8) +
  geom_line(aes(y = option, color = "Option leg"), linewidth = 0.8) +
  geom_line(aes(y = combo_same_time, color = "Combined (same settlement)"), linewidth = 0.9) +
  geom_vline(xintercept = c(pm_lower, K, pm_upper), linetype = "dotted", linewidth = 0.3) +
  scale_color_manual(
    values = c(
      "Digital leg" = "#B35C44",
      "Option leg" = "#4F6D7A",
      "Combined (same settlement)" = "#355C7D"
    )
  ) +
  labs(
    title = "Same-settlement payoff comparison",
    subtitle = "A stylized digital breakout contract plus a long straddle",
    x = "Settlement price",
    y = "Payoff (USDT)",
    color = NULL
  )
p1 <- investlabr::viz_theme_apply(
  p1,
  style = style_name,
  context = context_name,
  legend_position = "bottom",
  show_compiler = FALSE
)

p2 <- ggplot(heat, aes(S7, ST)) +
  geom_raster(aes(fill = payoff), interpolate = TRUE) +
  geom_contour(aes(z = payoff), breaks = 0, color = "black", linewidth = 0.3) +
  geom_vline(xintercept = c(pm_lower, pm_upper), linetype = "dashed", linewidth = 0.3) +
  geom_hline(yintercept = K, linetype = "dashed", linewidth = 0.3) +
  scale_fill_gradient2(low = "#B35C44", mid = "#F8F8F4", high = "#355C7D", midpoint = 0) +
  labs(
    title = "Settlement-mismatch payoff heatmap",
    subtitle = "Polymarket-style digital settles on S7 while the option settles later on ST",
    x = "S7",
    y = "ST",
    fill = "Payoff"
  )
p2 <- investlabr::viz_theme_apply(
  p2,
  style = style_name,
  context = context_name,
  legend_position = "right",
  show_compiler = FALSE
)

p3 <- ggplot(risk_dt, aes(S7)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(aes(y = payoff_mean, color = "Mean"), linewidth = 0.8) +
  geom_line(aes(y = payoff_median, color = "Median"), linetype = "dashed", linewidth = 0.7) +
  geom_line(aes(y = payoff_q, color = "1% tail"), linetype = "dotted", linewidth = 0.8) +
  geom_vline(xintercept = c(pm_lower, pm_upper), linetype = "dashed", linewidth = 0.3) +
  scale_color_manual(values = c("Mean" = "#355C7D", "Median" = "#6E8CA6", "1% tail" = "#B35C44")) +
  labs(
    title = "Conditional risk view by digital-settlement price",
    subtitle = sprintf("GBM bridge from S7 to ST with sigma = %.2f and %d days between settlements", sigma_annual, days_between),
    x = "S7",
    y = "Payoff statistic (USDT)",
    color = NULL
  )
p3 <- investlabr::viz_theme_apply(
  p3,
  style = style_name,
  context = context_name,
  legend_position = "bottom",
  show_compiler = FALSE
)

p4 <- ggplot(grid, aes(S)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(aes(y = valley_option, color = "Option loss valley"), linewidth = 0.8) +
  geom_line(aes(y = valley_combo, color = "Combined loss valley"), linetype = "dashed", linewidth = 0.9) +
  geom_vline(xintercept = c(pm_lower, K, pm_upper), linetype = "dotted", linewidth = 0.3) +
  scale_color_manual(values = c("Option loss valley" = "#4F6D7A", "Combined loss valley" = "#B35C44")) +
  labs(
    title = "Loss-valley comparison",
    subtitle = "The digital leg can soften the straddle loss valley near the center of the range",
    x = "Settlement price",
    y = "Negative payoff only",
    color = NULL
  )
p4 <- investlabr::viz_theme_apply(
  p4,
  style = style_name,
  context = context_name,
  legend_position = "bottom",
  show_compiler = FALSE
)

same_min <- min(grid$combo_same_time, na.rm = TRUE)
heat_min <- min(heat$payoff, na.rm = TRUE)
tail_min <- min(risk_dt$payoff_q, na.rm = TRUE)

bottom_text <- paste0(
  "Scenario note: this board is a stylized payoff study, not execution advice or a live arbitrage detector. ",
  "The key research question is settlement mismatch: if the digital leg resolves on S7 but the option settles later on ST, ",
  "the same-settlement payoff shape can materially overstate protection. ",
  "Diagnostics: same-settlement minimum = ", sprintf("%.1f", same_min),
  ", deterministic heatmap minimum = ", sprintf("%.1f", heat_min),
  ", conditional 1% tail minimum = ", sprintf("%.1f", tail_min), "."
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p1, p2, p3, p4),
  n_rows = 2,
  n_cols = 2,
  title = "Simulation: digital-plus-option settlement mismatch board",
  bottom = bottom_text,
  style = style_name,
  context = context_name,
  show_compiler = TRUE
)
