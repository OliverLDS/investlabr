# Gallery script template
#
# Purpose:
# Data source:
# Main functions demonstrated:
# Promotion candidate:

library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

style <- "policy_memo"
context <- "report"

# Optional sync if your local cache is not up to date.
# investdatar::sync_local_fred_data("SERIES_ID")
# investdatar::sync_local_quantmod_OHLC("SPY", src = "yahoo")

# ---- Load local data ----

# Replace this block with the relevant local-cache provider.
dt <- data.table::as.data.table(investdatar::get_local_FRED_data("SERIES_ID"))
dt[, date := as.Date(date)]
dt[, value := as.numeric(value)]
dt <- dt[!is.na(date) & !is.na(value)]

# ---- Prepare research table ----

plot_dt <- data.table::copy(dt)

# ---- Build plot ----

p <- ggplot(plot_dt, aes(x = date, y = value)) +
  geom_line(
    color = investlabr::viz_style_get(style, context)$accent,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  labs(
    title = "Replace With Research-Facing Title",
    subtitle = "Replace with one sentence explaining the plot.",
    x = NULL,
    y = NULL
  )

p <- investlabr::viz_theme_apply(
  p,
  style = style,
  context = context
)

cat(
  paste0(
    "Replace this paragraph with a short reader-facing explanation of the research question, ",
    "data source, and the main conclusion or caveat.\n\n"
  )
)

print(p)
