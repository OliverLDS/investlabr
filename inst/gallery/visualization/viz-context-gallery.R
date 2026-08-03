library(data.table)
library(ggplot2)
library(gridExtra)
library(investlabr)

contexts <- c("report", "slide", "dashboard")
style_name <- "macro_classic"

demo_dt <- data.table(
  date = seq.Date(as.Date("2025-01-01"), by = "month", length.out = 16)
)
demo_dt[, policy_rate := c(4.75, 4.75, 4.50, 4.50, 4.25, 4.25, 4.25, 4.00, 4.00, 3.75, 3.75, 3.50, 3.50, 3.50, 3.25, 3.25)]
demo_dt[, two_year := c(4.40, 4.35, 4.20, 4.10, 4.05, 3.95, 3.90, 3.85, 3.80, 3.70, 3.65, 3.60, 3.55, 3.50, 3.45, 3.40)]
demo_dt[, ten_year := c(4.25, 4.30, 4.15, 4.05, 4.00, 3.95, 3.90, 3.88, 3.82, 3.78, 3.74, 3.70, 3.68, 3.65, 3.63, 3.60)]

long_dt <- melt(
  demo_dt,
  id.vars = "date",
  variable.name = "series",
  value.name = "value"
)

make_context_plot <- function(context_name) {
  p <- ggplot(long_dt, aes(x = date, y = value, color = series)) +
    geom_line(linewidth = 1) +
    labs(
      title = context_name,
      subtitle = paste("Style:", style_name),
      x = "",
      y = "Percent"
    ) +
    scale_color_manual(values = viz_palette_get(style = style_name, context = context_name, palette = "discrete")[1:3])
  viz_theme_apply(p, style = style_name, context = context_name)
}

context_gallery_plots <- stats::setNames(lapply(contexts, make_context_plot), contexts)

gridExtra::grid.arrange(
  grobs = context_gallery_plots,
  ncol = 1,
  top = "investlabr Context Gallery"
)
