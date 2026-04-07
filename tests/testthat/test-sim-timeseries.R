test_that("AR and MA simulation helpers return research-ready tables", {
  ar1_dt <- sim_ar1_dgp(n = 120, a1 = 0.5, sigma = 0.8, seed = 123)
  ar2_dt <- sim_ar2_dgp(n = 120, a1 = 0.4, a2 = -0.2, seed = 123)
  ar3_dt <- sim_ar3_dgp(n = 120, a1 = 0.4, a2 = -0.2, a3 = 0.1, seed = 123)
  ma1_dt <- sim_ma1_dgp(n = 120, b1 = 0.3, seed = 123)

  for (dt in list(ar1_dt, ar2_dt, ar3_dt, ma1_dt)) {
    testthat::expect_s3_class(dt, "data.table")
    testthat::expect_equal(nrow(dt), 120)
    testthat::expect_true(all(c("t", "value", "innovation") %in% names(dt)))
    testthat::expect_true(all(is.finite(dt$value)))
  }
})

test_that("ARCH and GARCH helpers include conditional variance output", {
  arch_dt <- sim_arch1_dgp(n = 150, alpha0 = 0.1, alpha1 = 0.2, seed = 42)
  garch_dt <- sim_garch11_dgp(n = 150, alpha0 = 0.1, alpha1 = 0.1, beta1 = 0.7, seed = 42)

  for (dt in list(arch_dt, garch_dt)) {
    testthat::expect_s3_class(dt, "data.table")
    testthat::expect_equal(nrow(dt), 150)
    testthat::expect_true(all(c("t", "value", "innovation", "sigma2") %in% names(dt)))
    testthat::expect_true(all(dt$sigma2 > 0))
  }
})

test_that("simulation helpers honor seed reproducibility", {
  dt1 <- sim_garch11_dgp(n = 80, alpha0 = 0.1, alpha1 = 0.1, beta1 = 0.7, seed = 7)
  dt2 <- sim_garch11_dgp(n = 80, alpha0 = 0.1, alpha1 = 0.1, beta1 = 0.7, seed = 7)

  testthat::expect_equal(dt1$value, dt2$value)
  testthat::expect_equal(dt1$sigma2, dt2$sigma2)
})
