test_that("viz_style_get resolves named styles and contexts", {
  style <- viz_style_get(style = "macro_classic", context = "slide")

  testthat::expect_type(style, "list")
  testthat::expect_equal(style$name, "macro_classic")
  testthat::expect_equal(style$context, "slide")
  testthat::expect_true(all(c("accent", "accent2", "discrete", "line_width") %in% names(style)))
})

test_that("styled event plot returns a ggplot object", {
  DT <- data.table::data.table(
    datetime = seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = 40),
    close = seq(100, 139),
    event_x = as.integer(seq_len(40) %% 10 == 0)
  )
  attr(DT, "inst_id") <- "TEST"

  res <- eval_event_performance(DT, "event_x", H = 1L:5L)
  p <- gen_plot_event_tsline_cum_ret(res, style = "presentation_bold", context = "slide")

  testthat::expect_s3_class(p, "ggplot")
})
