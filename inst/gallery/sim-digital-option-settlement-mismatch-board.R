library(data.table)
library(ggplot2)
library(investlabr)

set.seed(1)

# Stylized but recent-looking ETH-centered settings.
p_yes_up <- 0.015
p_yes_down <- 0.52
pm_upper <- 2300
pm_lower <- 2200
pm_units <- 300

K <- 2250
prem_call_market <- 42
prem_put_market <- 48
prem_call_ideal <- 105
prem_put_ideal <- 115
fee_trade <- 0
fee_exercise <- 0

S_min <- 1950
S_max <- 2550
n_grid <- 2001L

mu_annual <- 0
sigma_annual <- 0.80
days_between <- 7L
n_mc <- 4000L
quantile_q <- 0.01

pm_cost <- p_yes_up + p_yes_down

pm_payoff <- function(S7) {
  payout <- as.numeric(S7 > pm_upper | S7 < pm_lower)
  payout - pm_cost
}

short_straddle_payoff <- function(ST, prem_call, prem_put) {
  premium_collected <- prem_call + prem_put
  intrinsic_paid <- pmax(ST - K, 0) + pmax(K - ST, 0)
  premium_collected - intrinsic_paid - fee_trade - fee_exercise
}

combined_payoff <- function(S7, ST, prem_call, prem_put) {
  pm_units * pm_payoff(S7) + short_straddle_payoff(ST, prem_call = prem_call, prem_put = prem_put)
}

grid <- data.table(S = seq(S_min, S_max, length.out = n_grid))
grid[, `:=`(
  prediction_market = pm_units * pm_payoff(S),
  short_straddle_market = short_straddle_payoff(S, prem_call_market, prem_put_market),
  short_straddle_ideal = short_straddle_payoff(S, prem_call_ideal, prem_put_ideal)
)]
grid[, `:=`(
  combined_market_same_time = prediction_market + short_straddle_market,
  combined_ideal_same_time = prediction_market + short_straddle_ideal,
  loss_market_same_time = pmin(prediction_market + short_straddle_market, 0),
  loss_ideal_same_time = pmin(prediction_market + short_straddle_ideal, 0)
)]

S7_vals <- seq(S_min, S_max, length.out = 161L)
ST_vals <- seq(S_min, S_max, length.out = 161L)

heat <- CJ(S7 = S7_vals, ST = ST_vals)
heat[, payoff := combined_payoff(S7, ST, prem_call = prem_call_ideal, prem_put = prem_put_ideal)]

simulate_conditional <- function(S7_vec, mu_annual, sigma_annual, days_between, prem_call, prem_put, n_mc = 20000L, q = 0.01) {
  T_years <- days_between / 365
  out <- vector("list", length(S7_vec))

  for (i in seq_along(S7_vec)) {
    S7 <- S7_vec[i]
    Z <- rnorm(n_mc)
    ST <- S7 * exp((mu_annual - 0.5 * sigma_annual^2) * T_years + sigma_annual * sqrt(T_years) * Z)
    pay <- combined_payoff(S7, ST, prem_call = prem_call, prem_put = prem_put)

    out[[i]] <- data.table(
      S7 = S7,
      payoff_mean = mean(pay),
      payoff_median = median(pay),
      payoff_q = as.numeric(stats::quantile(pay, probs = q))
    )
  }

  rbindlist(out)
}

risk_dt <- simulate_conditional(
  S7_vec = S7_vals,
  mu_annual = mu_annual,
  sigma_annual = sigma_annual,
  days_between = days_between,
  prem_call = prem_call_ideal,
  prem_put = prem_put_ideal,
  n_mc = n_mc,
  q = quantile_q
)

style_name <- "institutional_blue"
context_name <- "report"

same_min_market <- min(grid$combined_market_same_time, na.rm = TRUE)
same_min_ideal <- min(grid$combined_ideal_same_time, na.rm = TRUE)
heat_min <- min(heat$payoff, na.rm = TRUE)
tail_min <- min(risk_dt$payoff_q, na.rm = TRUE)

story_paragraph <- paste(
  "Narrative:",
  "This board studies a stylized ETH structure that combines a prediction-market breakout trade with a short straddle.",
  paste0(
    "The prediction-market leg settles on S7, the ETH price on day 7, and pays if ETH finishes outside the [",
    pm_lower, ", ", pm_upper, "] range."
  ),
  paste0(
    "The option leg is a short straddle struck at K = ", K,
    ", but it settles later on ST, the ETH price on the final option expiry date."
  ),
  paste0(
    "The numbers are intentionally stylized around a recent ETH regime above 2,000.",
    " The market-premium case uses a normal-looking short-straddle premium total of ",
    prem_call_market + prem_put_market,
    ", which does not create a clean same-settlement arbitrage."
  ),
  paste0(
    "The ideal case lifts the short-straddle premium total to ", prem_call_ideal + prem_put_ideal,
    " to show the pricing region where the same-settlement sketch can look unusually attractive."
  ),
  "Panel 1 explains the breakout leg on its own. Panel 2 shows that only the idealized premium case creates a convincing same-settlement cushion.",
  paste0(
    "Panel 3 then shows the real issue: once S7 and ST diverge, the payoff depends on both dates.",
    " Panel 4 adds a zero-drift GBM bridge with ", sprintf("%.0f%%", 100 * sigma_annual),
    " annualized volatility over ", days_between, " days to show how tail risk reappears even in the ideal pricing case."
  ),
  paste0(
    "The main conclusion is that an apparent same-settlement edge can disappear once settlement mismatch is introduced.",
    " In this setup the ideal same-settlement minimum is ", sprintf("%.1f", same_min_ideal),
    ", but the deterministic mismatch minimum falls to ", sprintf("%.1f", heat_min),
    " and the simulated 1% tail reaches ", sprintf("%.1f", tail_min), "."
  )
)

p1 <- ggplot(grid, aes(S)) +
  geom_rect(
    xmin = S_min,
    xmax = pm_lower,
    ymin = -Inf,
    ymax = Inf,
    fill = "#F4E5DE",
    alpha = 0.55
  ) +
  geom_rect(
    xmin = pm_upper,
    xmax = S_max,
    ymin = -Inf,
    ymax = Inf,
    fill = "#F4E5DE",
    alpha = 0.55
  ) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(aes(y = prediction_market), color = "#355C7D", linewidth = 0.95) +
  geom_vline(xintercept = c(pm_lower, pm_upper), linetype = "dotted", linewidth = 0.3) +
  labs(
    title = "Prediction-market breakout payoff",
    subtitle = "The leg wins only if ETH finishes outside the 2200 to 2300 range on S7",
    x = "S7 settlement price",
    y = "Payoff (USDT)"
  )
p1 <- investlabr::viz_theme_apply(
  p1,
  style = style_name,
  context = context_name,
  show_compiler = FALSE
)

p2 <- ggplot(grid, aes(S)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(aes(y = short_straddle_market, color = "Short straddle only"), linewidth = 0.8) +
  geom_line(aes(y = combined_market_same_time, color = "Combined with market premium"), linewidth = 0.9) +
  geom_line(aes(y = combined_ideal_same_time, color = "Combined with ideal premium"), linewidth = 0.95) +
  geom_vline(xintercept = c(pm_lower, K, pm_upper), linetype = "dotted", linewidth = 0.3) +
  scale_color_manual(
    values = c(
      "Short straddle only" = "#6E8CA6",
      "Combined with market premium" = "#B35C44",
      "Combined with ideal premium" = "#355C7D"
    )
  ) +
  labs(
    title = "Same-settlement value depends on option pricing",
    subtitle = "A normal-looking short-straddle premium does not create a clean edge; an idealized premium can",
    x = "Common settlement price",
    y = "Payoff (USDT)",
    color = NULL
  )
p2 <- investlabr::viz_theme_apply(
  p2,
  style = style_name,
  context = context_name,
  legend_position = "bottom",
  show_compiler = FALSE
)

p3 <- ggplot(heat, aes(S7, ST)) +
  geom_raster(aes(fill = payoff), interpolate = TRUE) +
  geom_contour(aes(z = payoff), breaks = 0, color = "black", linewidth = 0.3) +
  geom_vline(xintercept = c(pm_lower, pm_upper), linetype = "dashed", linewidth = 0.3) +
  geom_hline(yintercept = K, linetype = "dashed", linewidth = 0.3) +
  scale_fill_gradient2(low = "#B35C44", mid = "#F8F8F4", high = "#355C7D", midpoint = 0) +
  labs(
    title = "Settlement mismatch changes the payoff map",
    subtitle = "The attractive same-settlement picture breaks once S7 and ST become different prices",
    x = "S7: prediction-market settlement price",
    y = "ST: option settlement price",
    fill = "Payoff"
  )
p3 <- investlabr::viz_theme_apply(
  p3,
  style = style_name,
  context = context_name,
  legend_position = "right",
  show_compiler = FALSE
)

p4 <- ggplot(risk_dt, aes(S7)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(aes(y = payoff_mean, color = "Mean"), linewidth = 0.8) +
  geom_line(aes(y = payoff_median, color = "Median"), linetype = "dashed", linewidth = 0.75) +
  geom_line(aes(y = payoff_q, color = "1% tail"), linetype = "dotted", linewidth = 0.85) +
  geom_vline(xintercept = c(pm_lower, pm_upper), linetype = "dashed", linewidth = 0.3) +
  scale_color_manual(values = c("Mean" = "#355C7D", "Median" = "#6E8CA6", "1% tail" = "#B35C44")) +
  labs(
    title = "Even the ideal case keeps bridge risk",
    subtitle = sprintf("From S7 to ST, a zero-drift GBM bridge with %.0f%% annualized volatility over %d days still leaves tail losses", 100 * sigma_annual, days_between),
    x = "S7: prediction-market settlement price",
    y = "Payoff statistic (USDT)",
    color = NULL
  )
p4 <- investlabr::viz_theme_apply(
  p4,
  style = style_name,
  context = context_name,
  legend_position = "bottom",
  show_compiler = FALSE
)

bottom_text <- paste0(
  "Scenario note: this board is a stylized payoff study, not execution advice or a live arbitrage detector. ",
  "The numerical settings use a recent ETH-centered range so the geometry is easy to read. ",
  "S7 is the prediction-market settlement price and ST is the later option-settlement price. ",
  "The subplot logic moves from the standalone breakout leg, to same-settlement pricing, to deterministic mismatch, to simulated bridge risk. ",
  "Diagnostics: market-premium same-settlement minimum = ", sprintf("%.1f", same_min_market),
  ", ideal same-settlement minimum = ", sprintf("%.1f", same_min_ideal),
  ", deterministic mismatch minimum = ", sprintf("%.1f", heat_min),
  ", conditional 1% tail minimum = ", sprintf("%.1f", tail_min), "."
)

plot_obj <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(p1, p2, p3, p4),
  n_rows = 2,
  n_cols = 2,
  title = "How Settlement Mismatch Changes a Breakout-Plus-Short-Straddle Idea",
  bottom = bottom_text,
  style = style_name,
  context = context_name,
  show_compiler = TRUE
)

cat(paste(strwrap(story_paragraph, width = 100), collapse = "\n"), "\n")
