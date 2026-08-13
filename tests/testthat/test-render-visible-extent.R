make_extent_test_panel <- function(last_date) {
  dates <- seq(as.Date("2026-06-01"), as.Date(last_date), by = "day")
  dt <- data.frame(
    datetime = as.POSIXct(dates, tz = "UTC"),
    value = seq_along(dates)
  )
  ggplot2::ggplot(dt, ggplot2::aes(x = datetime, y = value)) +
    ggplot2::geom_line(linewidth = 0.9, colour = "#2F5D7C") +
    ggplot2::scale_x_datetime(
      limits = as.POSIXct(c("2026-06-01", "2026-08-20"), tz = "UTC"),
      date_breaks = "1 month",
      date_labels = "%Y-%m",
      timezone = "UTC"
    ) +
    ggplot2::theme_minimal()
}

test_that("SVG extent check accepts paths through each latest observation", {
  skip_if_not_installed("svglite")
  latest <- rep(as.Date("2026-08-10"), 4L)
  panel <- make_extent_test_panel(latest[1L])
  board <- gen_grid_of_plots_with_labels(
    plots = rep(list(panel), 4L),
    n_rows = 2L,
    n_cols = 2L,
    show_compiler = FALSE
  )
  path <- tempfile(fileext = ".svg")
  ggplot2::ggsave(path, board, width = 12, height = 8)

  result <- .brief_svg_check_time_extent(path, latest, 2L, 2L)
  expect_identical(result$visible_data_as_of, "2026-08-10")
  expect_true(all(result$panels$visible_x + 3 >= result$panels$expected_x))
})

test_that("SVG extent check rejects materially stale visible paths", {
  skip_if_not_installed("svglite")
  panel <- make_extent_test_panel("2026-07-20")
  board <- gen_grid_of_plots_with_labels(
    plots = rep(list(panel), 4L),
    n_rows = 2L,
    n_cols = 2L,
    show_compiler = FALSE
  )
  path <- tempfile(fileext = ".svg")
  ggplot2::ggsave(path, board, width = 12, height = 8)

  expect_error(
    .brief_svg_check_time_extent(
      path,
      rep(as.Date("2026-08-10"), 4L),
      2L,
      2L
    ),
    "Rendered data paths end before the latest observation"
  )
})
