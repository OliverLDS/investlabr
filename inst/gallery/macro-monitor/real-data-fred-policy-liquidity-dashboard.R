library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

series_ids <- c("DFEDTARL", "DFEDTARU", "EFFR", "IORB", "SOFR", "WALCL", "WSHOTSA", "WMBSEC", "WRESBAL")

# Optional if you have not synced locally yet:
# invisible(lapply(series_ids, investdatar::sync_local_fred_data))

get_fred_local <- function(id, label = id) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(id))
  dt[, date := as.Date(date)]
  dt[, value := as.numeric(value)]
  dt[, series := label]
  dt[!is.na(value)]
}

cut_since <- function(dt, from = as.Date("2010-01-01")) {
  dt[date >= from]
}

df_tarl <- get_fred_local("DFEDTARL", "Target Lower")
df_taru <- get_fred_local("DFEDTARU", "Target Upper")
df_effr <- get_fred_local("EFFR", "EFFR")
df_iorb <- get_fred_local("IORB", "IORB")
df_sofr <- get_fred_local("SOFR", "SOFR")

policy_band <- merge(
  df_tarl[, .(date, lower = value)],
  df_taru[, .(date, upper = value)],
  by = "date",
  all = TRUE
)
st_rates <- rbindlist(list(df_effr, df_iorb, df_sofr), use.names = TRUE)

policy_band <- cut_since(policy_band, as.Date("2015-01-01"))
st_rates <- cut_since(st_rates, as.Date("2015-01-01"))

p_policy <- ggplot() +
  geom_ribbon(
    data = policy_band,
    aes(x = date, ymin = lower, ymax = upper),
    alpha = 0.15,
    fill = investlabr::viz_style_get("policy_memo", "report")$accent
  ) +
  geom_line(
    data = st_rates,
    aes(x = date, y = value, color = series),
    linewidth = 0.5
  ) +
  scale_color_manual(values = investlabr::viz_palette_get("policy_memo", "report", "discrete")[1:3]) +
  scale_y_continuous("Policy / Overnight Rates (%)") +
  labs(
    title = "Fed Target Range and Overnight Rates",
    subtitle = "EFFR, IORB, and SOFR versus the target band",
    x = NULL,
    y = NULL
  )
p_policy <- investlabr::viz_theme_apply(
  p_policy,
  style = "policy_memo",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

walcl <- get_fred_local("WALCL", "Total Assets")
wshot <- get_fred_local("WSHOTSA", "USTs (held outright)")
wmbsec <- get_fred_local("WMBSEC", "MBS (held outright)")
wresbal <- get_fred_local("WRESBAL", "Reserve Balances")

balance_sheet <- rbindlist(list(walcl, wshot, wmbsec, wresbal), use.names = TRUE)
balance_sheet <- cut_since(balance_sheet, as.Date("2010-01-01"))
balance_sheet[, value_trn := value / 1e6]

p_balance <- ggplot(balance_sheet, aes(x = date, y = value_trn, color = series)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = investlabr::viz_palette_get("policy_memo", "report", "discrete")[1:4]) +
  scale_y_continuous("Level (trillions USD)", labels = scales::label_number(accuracy = 0.1)) +
  labs(
    title = "Fed Balance Sheet and Reserve Balances",
    subtitle = "Total assets, USTs, MBS, and reserves",
    x = NULL,
    y = NULL
  )
p_balance <- investlabr::viz_theme_apply(
  p_balance,
  style = "policy_memo",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_policy, p_balance),
  n_rows = 2,
  n_cols = 1,
  title = "Policy Rates and Liquidity Dashboard",
  bottom = "FRED inputs: DFEDTARL, DFEDTARU, EFFR, IORB, SOFR, WALCL, WSHOTSA, WMBSEC, WRESBAL.",
  style = "policy_memo",
  context = "report"
)
