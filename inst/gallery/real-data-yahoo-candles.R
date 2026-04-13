library(data.table)
library(investdatar)
library(investlabr)
library(strategyr)

ticker <- "^GSPC"
from_date <- as.Date("2024-01-01")
to_date <- Sys.Date()
window_length <- 400L
cycle_n <- 180L

# Optional sync if your local Yahoo cache is not up to date.
# investdatar::sync_local_quantmod_OHLC(
#   ticker = ticker,
#   from = as.character(from_date - 365L),
#   to = as.character(to_date),
#   src = "yahoo"
# )

build_sr_points_from_strategyr_cycle <- function(DT, n_support = 3L, n_resistance = 3L) {
  work_dt <- data.table::copy(DT)

  strategyr::calc_EMA(work_dt, ns = c(20L, 50L, 100L, 200L))
  strategyr::calc_ATR(work_dt, ns = 14L, hs = NULL)

  latest <- work_dt[.N]
  price_now <- latest$close

  # These are current strategyr internals. The legacy public helpers live in
  # strategyr/temporary/R/calc_sr_now.R and should ideally be promoted back into
  # strategyr before production use.
  pivots <- data.table::as.data.table(
    strategyr:::detect_pivots_cpp(
      work_dt$high,
      work_dt$low,
      work_dt$datetime,
      span = 3L,
      latest_n = NULL,
      refined = TRUE,
      min_swing = 0.05
    )
  )
  cycles <- data.table::as.data.table(strategyr:::detect_cycles_now_cpp(pivots))
  if (nrow(cycles) == 0L) {
    stop("Could not derive support/resistance levels because strategyr detected no current cycles.")
  }

  cycles[, contains_now := (price_now >= bg_price & price_now <= ed_price) |
    (price_now <= bg_price & price_now >= ed_price)]
  if (any(cycles$contains_now)) {
    cycles <- cycles[contains_now == TRUE]
  }
  cycles[, cycle_id := .I]
  cycles[, fresh := .I == .N]
  cycles[, main := .I == 1L]

  check_proximity <- function(base, value, threshold = 0.002) {
    abs(value / base - 1) < threshold
  }

  fibs <- data.table::rbindlist(
    lapply(seq_len(nrow(cycles)), function(i) {
      pts <- data.table::as.data.table(
        strategyr:::fib_all_cpp(cycles$bg_price[i], cycles$ed_price[i])$points
      )
      pts[, `:=`(
        cycle_id = cycles$cycle_id[i],
        cycle_fresh = cycles$fresh[i],
        cycle_main = cycles$main[i],
        cycle_dir = cycles$direction[i]
      )]
      pts
    }),
    use.names = TRUE,
    fill = TRUE
  )

  fibs <- fibs[is.finite(level) & level > 0]
  fibs[, sr := data.table::fifelse(level < price_now, "support", "resistance")]
  fibs[, ema20 := check_proximity(latest$ema_20, level)]
  fibs[, ema50 := check_proximity(latest$ema_50, level)]
  fibs[, ema100 := check_proximity(latest$ema_100, level)]
  fibs[, flipped := sr_hint == sr]
  fibs[, score := 0]
  fibs[, score := score + data.table::fifelse(ema20 | ema50 | ema100, 3^7, 0)]
  fibs[, score := score + data.table::fifelse(cycle_fresh & group == "retr", 3^6, 0)]
  fibs[, score := score + data.table::fifelse(cycle_main & group == "retr", 3^5, 0)]
  fibs[, score := score + data.table::fifelse(cycle_fresh & group != "retr", 3^4, 0)]
  fibs[, score := score + data.table::fifelse(cycle_main & group != "retr", 3^3, 0)]
  fibs[, score := score + data.table::fifelse(label %in% c("61.8%", "38.2%"), 3^2, 0)]
  fibs[, score := score + data.table::fifelse(flipped, 3^1, 0)]
  scored_levels <- fibs[, .(score = sum(score, na.rm = TRUE)), by = .(level, sr)]

  tol_gap <- if ("atr_14" %in% names(latest) && is.finite(latest$atr_14)) {
    latest$atr_14 / 5
  } else {
    diff(range(work_dt$close, na.rm = TRUE)) * 0.01
  }

  collapse_scored_levels <- function(levels_dt) {
    levels_dt <- levels_dt[order(level)]
    if (!nrow(levels_dt)) {
      return(levels_dt)
    }
    levels_dt[, zone := cumsum(c(TRUE, diff(level) > tol_gap))]
    levels_dt[
      ,
      .(
        level = weighted.mean(level, w = pmax(score, 1), na.rm = TRUE),
        score = sum(score, na.rm = TRUE),
        n = .N
      ),
      by = zone
    ][order(-score)]
  }

  support_pts <- collapse_scored_levels(scored_levels[sr == "support"])[seq_len(min(.N, n_support))][order(-level)]$level
  resistance_pts <- collapse_scored_levels(scored_levels[sr == "resistance"])[seq_len(min(.N, n_resistance))][order(level)]$level

  list(
    DT = work_dt,
    support_pts = sort(support_pts),
    resistance_pts = sort(resistance_pts)
  )
}

spy_dt <- data.table::as.data.table(investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo"))
spy_dt <- spy_dt[
  datetime >= as.POSIXct(from_date - 365L) &
    datetime < as.POSIXct(to_date + 1)
][order(datetime)]
spy_dt <- spy_dt[
  is.finite(open) &
    is.finite(high) &
    is.finite(low) &
    is.finite(close) &
    open > 0 &
    high > 0 &
    low > 0 &
    close > 0
]

if (nrow(spy_dt) < cycle_n) {
  stop("Need at least ", cycle_n, " valid Yahoo OHLC rows for the strategyr cycle-based S/R example.")
}

sr_res <- build_sr_points_from_strategyr_cycle(spy_dt)
plot_dt <- tail(sr_res$DT, window_length)

month_floor <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}
quarter_floor <- function(x) {
  x <- month_floor(x)
  y <- as.integer(format(x, "%Y"))
  m <- as.integer(format(x, "%m"))
  q_m <- ((m - 1L) %/% 3L) * 3L + 1L
  as.Date(sprintf("%s-%02d-01", y, q_m))
}

quarter_breaks <- as.POSIXct(
  seq(quarter_floor(min(plot_dt$datetime)), month_floor(max(plot_dt$datetime)), by = "3 months"),
  tz = "UTC"
)
monthly_guides <- as.POSIXct(
  seq(month_floor(min(plot_dt$datetime)), month_floor(max(plot_dt$datetime)), by = "1 month"),
  tz = "UTC"
)
plot_caption <- investlabr:::.investlabr_compiler_caption(paste0(
  "Data source: Yahoo Finance via investdatar. ",
  "Monthly vertical guides; x-axis labels shown quarterly. ",
  "Support/resistance levels are research-side references, not trading recommendations."
))

p <- investlabr::gen_candle_plots_with_sr_lines(
  plot_dt,
  support_pts = sr_res$support_pts,
  resistance_pts = sr_res$resistance_pts,
  show_ema_lines = TRUE,
  style = "research_note",
  context = "report"
) +
  ggplot2::geom_vline(
    xintercept = monthly_guides,
    color = "#9CA3AF",
    linewidth = 0.25,
    alpha = 0.25
  ) +
  ggplot2::scale_x_datetime(
    breaks = quarter_breaks,
    date_labels = "%Y-%m"
  ) +
  ggplot2::labs(
    title = "S&P 500 Candle Chart With Strategyr-Derived Support and Resistance",
    subtitle = "Support and resistance levels are scored from recent pivot cycles, Fibonacci levels, and EMA confluence.",
    caption = plot_caption
  )

print(p)
