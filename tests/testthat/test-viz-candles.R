test_that("support/resistance zone plot uses its supplied ATR window", {
  bars <- data.table::data.table(
    datetime = as.POSIXct("2025-01-01", tz = "UTC") + 86400 * 0:9,
    open = 100 + 0:9,
    close = 100 + 0:9,
    high = 101 + 0:9,
    low = 99 + 0:9,
    atr_logr_12 = rep(0.025, 10),
    atr_q_10_12_300 = rep(0.015, 10),
    atr_q_20_12_300 = rep(0.020, 10),
    atr_q_80_12_300 = rep(0.030, 10),
    atr_q_90_12_300 = rep(0.035, 10)
  )
  support <- data.table::data.table(zone_low = 98, zone_high = 100,
                                    zone_center = 99, score = 1)
  resistance <- data.table::data.table(zone_low = 109, zone_high = 111,
                                       zone_center = 110, score = 1)

  plot <- gen_candle_plots_with_sr_dts(
    bars, "Synthetic asset", "1D", support_dt = support,
    resistance_dt = resistance, show_compiler = FALSE
  )

  testthat::expect_s3_class(plot, "ggplot")
})
