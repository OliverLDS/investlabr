test_that("eval_strat_performance returns a one-row backtest summary", {
  DT <- data.table::data.table(
    datetime = seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = 20),
    open = seq(100, 119),
    close = seq(101, 120),
    pos_demo = rep(c(0, 1), length.out = 20)
  )
  attr(DT, "inst_id") <- "ASSET"
  attr(DT$pos_demo, "strat_name") <- "demo"
  attr(DT$pos_demo, "strat_par") <- list(window = 5)

  res <- eval_strat_performance(DT, "pos_demo")

  testthat::expect_s3_class(res, "data.table")
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_true(all(c("annual_return", "max_drawdown", "log_ret_dt") %in% names(res)))
  testthat::expect_s3_class(res$log_ret_dt[[1]], "data.table")
})
