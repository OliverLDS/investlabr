test_that("get_yield_data_DT builds maturity table", {
  yield_dt_list <- replicate(
    11,
    data.table::data.table(
      date = as.Date(c("2024-01-01", "2024-02-01")),
      value = c(5, 4.9)
    ),
    simplify = FALSE
  )

  res <- get_yield_data_DT(
    yield_dt_list = yield_dt_list,
    yield_dates = list(
      Now = as.Date("2024-02-01"),
      `One month ago` = as.Date("2024-01-01")
    )
  )

  testthat::expect_s3_class(res, "data.table")
  testthat::expect_equal(nrow(res), 11)
  testthat::expect_true(all(c("maturity", "log_maturity", "maturity_label", "Now", "One month ago") %in% names(res)))
})
