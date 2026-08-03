test_that("forecast preparation helpers produce stable research tables", {
  expect_equal(
    prep_fill_forward(c(NA_real_, 1, NA_real_, 2, NA_real_)),
    c(NA_real_, 1, 1, 2, 2)
  )

  series_a <- data.table::data.table(
    date = as.Date(c("2026-01-01", "2026-01-03")),
    value = c(1, 3),
    series = "A"
  )
  series_b <- data.table::data.table(
    date = as.Date(c("2026-01-02", "2026-01-03")),
    value = c(10, 11),
    series = "B"
  )
  wide <- prep_series_wide(list(series_a, series_b))

  expect_s3_class(wide, "data.table")
  expect_equal(wide$A, c(1, 1, 3))
  expect_equal(wide$B, c(NA_real_, 10, 11))
  expect_equal(mean(prep_zscore(1:5)), 0, tolerance = 1e-12)
  expect_equal(prep_recent_changes(1:10, lookback = 3L), rep(1, 3))
})

test_that("forecast simulation and score helpers are reproducible", {
  changes <- seq(-0.2, 0.2, length.out = 40)
  fan_1 <- sim_forward_fan(4, changes, horizons = c(1L, 5L), n_paths = 100L, seed = 7L)
  fan_2 <- sim_forward_fan(4, changes, horizons = c(1L, 5L), n_paths = 100L, seed = 7L)

  expect_equal(fan_1, fan_2)
  expect_equal(fan_1$horizon, c(0L, 1L, 5L))
  expect_true(all(c("p10", "p25", "p50", "p75", "p90") %in% names(fan_1)))
  expect_equal(
    sim_forward_fan(4, changes, horizons = 1L, n_paths = 1L, seed = 7L)$horizon,
    c(0L, 1L)
  )

  scenarios <- sim_scenario_path(
    start_value = 5,
    scenario_changes = c(Easing = -0.25, Hold = 0),
    horizon_months = 2L,
    start_date = as.Date("2026-01-01")
  )
  expect_equal(scenarios[scenario == "Easing", value], c(5, 4.75, 4.5))
  expect_equal(factor_bounded_score(0), 50)
  expect_identical(brief_score_label(75), "Latest: elevated (75/100)")
})

test_that("forecast visualization helpers return ggplot objects", {
  fan <- sim_forward_fan(4, seq(-0.2, 0.2, length.out = 40), horizons = c(1L, 5L), n_paths = 50L, seed = 2L)
  p_fan <- viz_forward_fan(
    fan,
    title = "Fan",
    subtitle = "Historical-change bootstrap",
    y_label = "Level",
    show_compiler = FALSE
  )
  line_dt <- data.table::data.table(
    date = rep(as.Date("2026-01-01") + 0:2, 2),
    series = rep(c("A", "B"), each = 3),
    value = 1:6
  )
  p_lines <- viz_series_lines(
    line_dt,
    x = "date",
    y = "value",
    title = "Lines",
    subtitle = "Two series",
    y_label = "Level",
    show_compiler = FALSE
  )

  expect_s3_class(p_fan, "ggplot")
  expect_s3_class(p_lines, "ggplot")
})
