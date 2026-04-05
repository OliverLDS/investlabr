library(data.table)
library(ggplot2)
library(grid)
library(investdatar)
library(investlabr)

# Optional if you have not synced locally yet:
# investdatar::sync_local_fred_data("EFFR")

style_name <- "policy_memo"
context_name <- "report"
style_spec <- investlabr::viz_style_get(style_name, context_name)

dt_effr <- data.table::as.data.table(investdatar::get_local_FRED_data("EFFR"))
dt_effr <- dt_effr[date >= as.Date("2000-01-01") & !is.na(value)]
dt_effr[, datetime := as.POSIXct(date, tz = "UTC")]

last_effr_date <- dt_effr[, max(date, na.rm = TRUE)]

trade_events <- data.table(
  start = as.Date(c(
    "2002-03-20",
    "2009-09-26",
    "2018-07-06",
    "2025-02-01"
  )),
  end = as.Date(c(
    "2003-12-04",
    "2012-09-25",
    "2020-01-15",
    as.character(last_effr_date)
  )),
  label_short = c(
    "Bush steel safeguards",
    "Obama China tire tariffs",
    "US-China trade war",
    "Trump tariffs 2.0"
  ),
  label_detail = c(
    "2002-2003",
    "2009-2012",
    "2018-2020",
    paste0("2025-", format(last_effr_date, "%Y"))
  ),
  type = c(
    "Major trade conflict",
    "Sectoral dispute",
    "Major trade conflict",
    "Major trade conflict"
  )
)

y_max <- dt_effr[, max(value, na.rm = TRUE)]
y_min <- dt_effr[, min(value, na.rm = TRUE)]
y_span <- y_max - y_min
if (!is.finite(y_span) || y_span <= 0) y_span <- 1

trade_events[, mid_date := start + (end - start) / 2]
trade_events[, label_y := seq(
  from = y_max * 0.90,
  to = y_max * 0.62,
  length.out = .N
)]
trade_events[, xmid := as.POSIXct(mid_date, tz = "UTC")]
trade_events[, xstart := as.POSIXct(start, tz = "UTC")]
trade_events[, xend := as.POSIXct(end, tz = "UTC")]

fill_vals <- c(
  "Major trade conflict" = "#D98C7A",
  "Sectoral dispute" = "#A7BFA3"
)

p <- ggplot() +
  geom_rect(
    data = trade_events,
    aes(
      xmin = xstart,
      xmax = xend,
      ymin = -Inf,
      ymax = Inf,
      fill = type
    ),
    alpha = 0.16,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = dt_effr,
    aes(x = datetime, y = value),
    linewidth = 0.7,
    color = style_spec$ink
  ) +
  geom_segment(
    data = trade_events,
    aes(
      x = xmid,
      xend = xmid,
      y = label_y - 0.12 * y_span,
      yend = y_max
    ),
    linewidth = 0.35,
    color = style_spec$muted,
    linetype = "dashed",
    arrow = arrow(length = grid::unit(2, "mm"), type = "closed")
  ) +
  geom_label(
    data = trade_events,
    aes(
      x = xmid,
      y = label_y,
      label = paste0(label_short, "\n", label_detail),
      fill = type
    ),
    size = 3.2,
    linewidth = 0.2,
    label.padding = grid::unit(0.18, "lines"),
    color = style_spec$ink,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = fill_vals) +
  labs(
    title = "EFFR with major and minor trade conflicts highlighted",
    subtitle = "Shaded windows mark selected US trade-conflict episodes since 2002",
    x = "",
    y = "Effective Federal Funds Rate (%)",
    caption = paste(
      "Interpretation:",
      "the figure is an event overlay rather than a causal model;",
      "it is meant to compare trade-conflict windows with the prevailing US policy-rate regime."
    )
  ) +
  scale_x_datetime(date_breaks = "2 years", date_labels = "%Y")

p <- investlabr::viz_theme_apply(
  p,
  style = style_name,
  context = context_name,
  legend_position = "bottom",
  show_compiler = TRUE
)

print(p)
