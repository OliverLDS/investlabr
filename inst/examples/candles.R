bars <- data.table::data.table(
  datetime = as.POSIXct("2025-01-01", tz = "UTC") + 86400 * 0:9,
  open = 100 + (0:9), close = 100 + (0:9) + sin(0:9)
)
bars[, high := pmax(open, close) + 1]
bars[, low := pmin(open, close) - 1]
viz_candle_base(bars, show_compiler = FALSE)
gen_candle_plots_with_sr_lines(bars, support_pts = 99, resistance_pts = 110)
support <- data.table::data.table(zone_low = 98, zone_high = 100,
                                  zone_center = 99, score = 1)
resistance <- data.table::data.table(zone_low = 109, zone_high = 111,
                                     zone_center = 110, score = 1)
gen_candle_plots_with_sr_dts(bars, "Synthetic asset", "1D",
                            support_dt = support, resistance_dt = resistance,
                            show_compiler = FALSE)
denominator <- data.table::copy(bars)
denominator[, c("open", "high", "low", "close") := lapply(.SD, function(x) x / 2),
            .SDcols = c("open", "high", "low", "close")]
head(prep_ratio_ohlc(bars, denominator, "A/B"))
