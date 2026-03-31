test_that("eval_event_performance returns expected list columns", {
  DT <- data.table::data.table(
    datetime = seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = 40),
    close = seq(100, 139),
    event_x = as.integer(seq_len(40) %% 10 == 0)
  )
  attr(DT, "inst_id") <- "TEST"

  res <- eval_event_performance(DT, "event_x", H = 1L:5L)

  testthat::expect_s3_class(res, "data.table")
  testthat::expect_true(all(c("performance_by_cases", "performance_by_horizons") %in% names(res)))
  testthat::expect_equal(nrow(res$performance_by_cases[[1]]), 4)
  testthat::expect_equal(nrow(res$performance_by_horizons[[1]]), 5)
})
