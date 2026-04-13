library(data.table)
library(ggplot2)
library(gridExtra)
library(investlabr)

styles <- c(
  "research_note",
  "macro_classic",
  "terminal_risk",
  "cross_asset_color",
  "minimal_print",
  "strategy_explain",
  "presentation_bold",
  "briefing_serif",
  "institutional_blue",
  "policy_memo",
  "desk_monitor",
  "client_slide",
  "newswire_print"
)

context_name <- "report"

dates <- seq.Date(as.Date("2025-01-01"), by = "month", length.out = 18)
demo_dt <- data.table(
  date = rep(dates, 3),
  asset = rep(c("Rates", "Equity", "Commodity"), each = length(dates))
)

demo_dt[asset == "Rates", value := cumsum(c(100, 0.6, -0.4, 0.7, 0.8, -0.5, 0.2, 0.4, 0.1, -0.3, 0.4, 0.2, -0.1, 0.5, 0.2, 0.3, -0.2, 0.2))]
demo_dt[asset == "Equity", value := cumsum(c(100, 1.4, 0.8, -1.1, 1.9, 1.1, -0.6, 1.0, 0.6, -0.8, 1.2, 0.7, -0.3, 1.4, 0.9, 0.5, -0.4, 0.8))]
demo_dt[asset == "Commodity", value := cumsum(c(100, 2.1, -0.5, 1.6, -1.2, 0.7, 1.0, -0.4, 0.8, 0.4, -0.7, 1.3, 0.6, -0.2, 0.9, 1.1, -0.3, 0.5))]

make_style_plot <- function(style_name) {
  p <- ggplot(demo_dt, aes(x = date, y = value, color = asset)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    labs(
      title = style_name,
      subtitle = paste("Context:", context_name),
      x = "",
      y = "Indexed Level"
    ) +
    scale_color_manual(values = viz_palette_get(style = style_name, context = context_name, palette = "discrete")[1:3])
  viz_theme_apply(p, style = style_name, context = context_name)
}

style_gallery_plots <- stats::setNames(lapply(styles, make_style_plot), styles)

gridExtra::grid.arrange(
  grobs = style_gallery_plots,
  ncol = 2,
  top = "investlabr Style Gallery"
)
