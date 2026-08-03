library(data.table)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(investdatar)
library(investlabr)

tickers <- c(
  "IVV", "IWM", "IEFA", "IEMG", "EWJ", "EWY", "EWT", "EWZ", "MCHI", "INDA",
  "AGG", "IEF", "TLT", "EMB", "HYG", "LQD",
  "IAU", "SLV", "IBIT", "ETHA",
  "SOXX", "IYW", "IYF", "IYE", "IBB", "ITA", "IYR", "USRT"
)

bg_date <- as.Date("2023-01-01")
ed_date <- Sys.Date() + 1

# Optional if you have not synced locally yet:
# invisible(lapply(tickers, investdatar::sync_local_ishare_data))

style_name <- "briefing_serif"
context_name <- "report"
style_spec <- investlabr::viz_style_get(style_name, context_name)

calc_ticker_stats <- function(ticker, bg_date, ed_date) {
  dt <- copy(investdatar::get_local_ishare_data(ticker))
  dt <- dt[date >= bg_date & date < ed_date]
  dt <- unique(dt, by = "date")
  dt <- dt[!is.na(log_ret)]
  if (nrow(dt) < 80L) return(NULL)

  simple_ret <- exp(dt$log_ret) - 1
  ann_return <- exp(mean(dt$log_ret, na.rm = TRUE) * 252) - 1
  ann_risk <- stats::sd(simple_ret, na.rm = TRUE) * sqrt(252)
  if (!is.finite(ann_return) || !is.finite(ann_risk) || ann_risk <= 0) return(NULL)

  data.table(
    ticker = ticker,
    ann_return = ann_return,
    ann_risk = ann_risk,
    return_risk = ann_return / ann_risk,
    obs = nrow(dt)
  )
}

pick_best <- function(dt, label, order_expr) {
  if (nrow(dt) == 0L) return(NULL)
  out <- copy(dt[eval(order_expr)][1])
  out[, angle := label]
  out
}

unique_angle_rows <- function(dt) {
  used <- character()
  out <- vector("list", nrow(dt))
  k <- 0L
  for (i in seq_len(nrow(dt))) {
    row <- dt[i]
    if (row$ticker %in% used) next
    k <- k + 1L
    out[[k]] <- row
    used <- c(used, row$ticker)
  }
  rbindlist(out[seq_len(k)], fill = TRUE)
}

compose_scatter_with_table <- function(scatter_plot, table_grob, headline, note) {
  ggplot() +
    coord_cartesian(xlim = c(0, 30), ylim = c(0, 16), expand = FALSE, clip = "off") +
    annotation_custom(
      grob = ggplotGrob(scatter_plot),
      xmin = 0.5, xmax = 21.2, ymin = 1.5, ymax = 15.0
    ) +
    annotation_custom(
      grob = table_grob,
      xmin = 21.8, xmax = 29.5, ymin = 3.0, ymax = 14.5
    ) +
    annotate(
      "text",
      x = 0.8,
      y = 15.6,
      hjust = 0,
      vjust = 1,
      label = headline,
      family = "",
      fontface = "bold",
      size = 5
    ) +
    annotate(
      "text",
      x = 0.8,
      y = 0.45,
      hjust = 0,
      vjust = 0,
      label = note,
      family = "",
      color = style_spec$muted,
      size = 3.1
    ) +
    theme_void()
}

mega_dt <- copy(investdatar::get_local_ishare_mega_data())
setnames(mega_dt, "Asset Class", "asset_class")
setnames(mega_dt, "Sub Asset Class", "sub_asset_class")
setnames(mega_dt, "Ticker", "ticker")
setnames(mega_dt, "Name", "fund_name")

stats_dt <- rbindlist(
  lapply(tickers, calc_ticker_stats, bg_date = bg_date, ed_date = ed_date),
  fill = TRUE
)

stats_dt <- mega_dt[
  stats_dt,
  on = "ticker"
][
  !is.na(asset_class)
]

asset_map <- c(
  "Commodity" = "Commodity",
  "Digital Assets" = "Digital Assets",
  "Equity" = "Equity",
  "Fixed Income" = "Fixed Income",
  "Real Estate" = "Real Estate"
)
stats_dt[, asset_class := fifelse(asset_class %in% names(asset_map), asset_map[asset_class], asset_class)]

label_tickers <- c(
  "IBIT", "ETHA", "IAU", "SLV", "SOXX", "IYW", "IYF", "IYE", "ITA",
  "TLT", "HYG", "LQD", "IEFA", "IEMG", "MCHI", "INDA", "IYR"
)

leader_dt <- rbindlist(list(
  pick_best(stats_dt, "Highest return", quote(order(-ann_return))),
  pick_best(stats_dt, "Best return / risk", quote(order(-return_risk))),
  pick_best(stats_dt, "Lowest risk", quote(order(ann_risk))),
  pick_best(stats_dt[asset_class == "Equity"], "Best equity", quote(order(-return_risk))),
  pick_best(stats_dt[asset_class == "Fixed Income"], "Best fixed income", quote(order(-return_risk))),
  pick_best(stats_dt[asset_class == "Commodity"], "Best commodity", quote(order(-return_risk))),
  pick_best(stats_dt[asset_class == "Digital Assets"], "Best digital asset", quote(order(-return_risk))),
  pick_best(stats_dt[asset_class == "Real Estate"], "Best real estate", quote(order(-return_risk)))
), fill = TRUE)

leader_dt <- unique_angle_rows(leader_dt)[
  ,
  .(
    Angle = angle,
    Ticker = ticker,
    `Asset Class` = asset_class,
    `Ann. Return` = sprintf("%.1f%%", ann_return * 100),
    `Ann. Risk` = sprintf("%.1f%%", ann_risk * 100)
  )
]

palette_vals <- c(
  "Commodity" = "#8B6F47",
  "Digital Assets" = "#B45F06",
  "Equity" = "#2E5B88",
  "Fixed Income" = "#5F7D6E",
  "Real Estate" = "#8A6D9E"
)

scatter_plot <- ggplot(
  stats_dt,
  aes(x = ann_risk, y = ann_return, color = asset_class)
) +
  geom_hline(yintercept = 0, color = style_spec$grid, linewidth = 0.4) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text_repel(
    data = stats_dt[ticker %in% label_tickers],
    aes(label = ticker),
    size = 3.2,
    box.padding = 0.25,
    point.padding = 0.2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_color_manual(values = palette_vals, drop = FALSE) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
      title = "Risk and return across a broader iShares opportunity set",
      subtitle = paste0(
      format(bg_date, "%Y-%m-%d"), " to ", format(ed_date - 1, "%Y-%m-%d"),
      " | Annualized from daily returns"
    ),
    x = "Annualized risk",
    y = "Annualized return",
    caption = NULL,
    color = NULL
  )

scatter_plot <- investlabr::viz_theme_apply(
  scatter_plot,
  style = style_name,
  context = context_name,
  legend_position = "bottom",
  show_compiler = FALSE
)

table_theme <- gridExtra::ttheme_minimal(
  base_size = 8,
  base_colour = style_spec$ink,
  base_family = "",
  core = list(
    bg_params = list(fill = style_spec$paper, col = NA),
    fg_params = list(hjust = 0, x = 0.02)
  ),
  colhead = list(
    bg_params = list(fill = scales::alpha(style_spec$grid, 0.35), col = NA),
    fg_params = list(fontface = "bold")
  )
)

table_grob <- gridExtra::tableGrob(leader_dt, rows = NULL, theme = table_theme)

note_text <- paste(
  "Interpretation:",
  "this is not an optimizer output.",
  "It is a cross-sectional map of where different iShares ETFs sat in the recent risk-return space,",
  "with a compact leaderboard showing standout funds from different angles.",
  sep = " "
)

opportunity_set_board <- compose_scatter_with_table(
  scatter_plot = scatter_plot,
  table_grob = table_grob,
  headline = "iShares opportunity set board",
  note = note_text
)

print(opportunity_set_board)
