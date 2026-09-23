data <- data.table::data.table(
  datetime = as.POSIXct("2025-01-01", tz = "UTC") + 86400 * 0:9,
  first = 1:10, second = 10:1
)
p <- ggplot2::ggplot(data, ggplot2::aes(datetime, first)) + ggplot2::geom_line()
board <- gen_grid_of_plots_with_labels(list(p, p), n_rows = 1, n_cols = 2,
                                      title = "Two views", show_compiler = FALSE)
grid::grid.newpage()
grid::grid.draw(board)
gen_facet_plot_from_multicol_ts(data, "datetime", c("first", "second"),
                               c("First", "Second"), show_compiler = FALSE)
