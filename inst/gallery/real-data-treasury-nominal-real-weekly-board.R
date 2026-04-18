library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

# Optional sync if your local Treasury cache is not up to date.
# investdatar::sync_local_treasury_rates("par_yield_curve")
# investdatar::sync_local_treasury_rates("real_yield_curve")

nominal_dt <- investdatar::get_local_treasury_rates("par_yield_curve")
real_dt <- investdatar::get_local_treasury_rates("real_yield_curve")

if (is.null(nominal_dt) || nrow(nominal_dt) == 0L) {
  stop("Local Treasury data not found for dataset: par_yield_curve")
}
if (is.null(real_dt) || nrow(real_dt) == 0L) {
  stop("Local Treasury data not found for dataset: real_yield_curve")
}

nominal_tenor_map <- data.table(
  tenor = c(
    "1MONTH", "1_5MONTH", "2MONTH", "3MONTH", "4MONTH", "6MONTH",
    "1YEAR", "2YEAR", "3YEAR", "5YEAR", "7YEAR", "10YEAR", "20YEAR", "30YEAR"
  ),
  maturity_years = c(1 / 12, 1.5 / 12, 2 / 12, 3 / 12, 4 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20, 30),
  maturity_label = c("1M", "1.5M", "2M", "3M", "4M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "20Y", "30Y")
)

real_tenor_map <- data.table(
  tenor = c("5YEAR", "7YEAR", "10YEAR", "20YEAR", "30YEAR"),
  maturity_years = c(5, 7, 10, 20, 30),
  maturity_label = c("5Y", "7Y", "10Y", "20Y", "30Y")
)

select_curve_dates <- function(real_dt, lookback_days = 92L) {
  stopifnot(is.data.table(real_dt))

  latest_date <- max(as.Date(real_dt$date), na.rm = TRUE)
  window_start <- latest_date - as.difftime(lookback_days, units = "days")

  real_10y_dt <- real_dt[
    measure == "yield" &
      tenor == "10YEAR" &
      date >= window_start &
      date <= latest_date,
    .(date = as.Date(date), value)
  ]

  if (nrow(real_10y_dt) == 0L) {
    stop("No real 10-year Treasury observations found in the lookback window.")
  }

  highest_dt <- real_10y_dt[which.max(value), as.Date(date)]
  lowest_dt <- real_10y_dt[which.min(value), as.Date(date)]

  c(
    latest = latest_date,
    highest_real_10y = highest_dt,
    lowest_real_10y = lowest_dt
  )
}

prep_curve_dt <- function(dt, tenor_map, curve_dates) {
  curve_dates <- as.Date(curve_dates)
  curve_date_labels <- setNames(names(curve_dates), format(curve_dates, "%Y-%m-%d"))

  dt_use <- dt[
    measure == "yield" & tenor %in% tenor_map$tenor,
    .(date = as.Date(date), tenor, value)
  ]
  data.table::setorder(dt_use, tenor, date)

  target_dt <- data.table::CJ(
    tenor = tenor_map$tenor,
    target_date = curve_dates,
    unique = TRUE
  )
  target_dt[, requested_date := target_date]

  out <- dt_use[
    target_dt,
    on = .(tenor, date = target_date),
    roll = Inf
  ]
  out[, target_date := requested_date]
  out[, requested_date := NULL]

  missing_before_start <- out[is.na(date) | is.na(value)]
  if (nrow(missing_before_start) > 0L) {
    stop(
      "Unable to backfill some Treasury tenor/date combinations. Missing requested pairs: ",
      paste(
        sprintf(
          "%s on %s",
          missing_before_start$tenor,
          format(missing_before_start$target_date, "%Y-%m-%d")
        ),
        collapse = ", "
      )
    )
  }

  backfilled_dt <- out[date < target_date]
  if (nrow(backfilled_dt) > 0L) {
    warning(
      paste(
        "Treasury curve example backfilled missing tenor values from the latest earlier available observation for:",
        paste(
          sprintf(
            "%s requested on %s (filled from %s)",
            backfilled_dt$tenor,
            format(backfilled_dt$target_date, "%Y-%m-%d"),
            format(backfilled_dt$date, "%Y-%m-%d")
          ),
          collapse = "; "
        )
      ),
      call. = FALSE
    )
  }

  out[, date := target_date]
  out <- merge(
    out[, .(date, tenor, value)],
    tenor_map,
    by = "tenor",
    all.x = TRUE,
    all.y = FALSE
  )
  out[, maturity_label := factor(maturity_label, levels = tenor_map$maturity_label, ordered = TRUE)]
  out[, curve_label := format(as.Date(date), "%Y-%m-%d")]
  data.table::setorder(out, date, maturity_years)
  list(data = out, curve_dates = curve_dates)
}

build_curve_plot <- function(curve_dt, title, subtitle, y_label) {
  y_guide_min <- floor(min(curve_dt$value, na.rm = TRUE) * 10) / 10
  y_guide_max <- ceiling(max(curve_dt$value, na.rm = TRUE) * 10) / 10
  y_tick_min <- floor(min(curve_dt$value, na.rm = TRUE) * 5) / 5
  y_tick_max <- ceiling(max(curve_dt$value, na.rm = TRUE) * 5) / 5
  y_breaks <- seq(y_tick_min, y_tick_max, by = 0.2)
  y_guides <- seq(y_guide_min, y_guide_max, by = 0.1)

  p <- ggplot(
    curve_dt,
    aes(x = maturity_label, y = value, color = curve_label, group = curve_label)
  ) +
    geom_hline(
      yintercept = y_guides,
      color = "#D9DEE5",
      linewidth = 0.28
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.1) +
    scale_y_continuous(breaks = y_breaks) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Maturity",
      y = y_label,
      color = NULL
    )

  investlabr::viz_theme_apply(
    p,
    style = "macro_classic",
    context = "report",
    legend_position = "bottom",
    show_compiler = FALSE
  )
}

build_forward_dt_from_curve <- function(curve_dt) {
  dt <- data.table::copy(curve_dt)
  data.table::setorder(dt, curve_label, maturity_years)

  dt[, rate_dec := value / 100]
  dt[, `:=`(
    prev_maturity_years = data.table::shift(maturity_years),
    prev_maturity_label = data.table::shift(as.character(maturity_label)),
    prev_rate_dec = data.table::shift(rate_dec)
  ), by = curve_label]
  dt <- dt[!is.na(prev_maturity_years)]

  # Adjacent forwards are practical approximations because Treasury par yields
  # are not bootstrapped zero-coupon spot rates.
  dt[, forward_dec := (
    ((1 + rate_dec)^maturity_years) /
      ((1 + prev_rate_dec)^prev_maturity_years)
  )^(1 / (maturity_years - prev_maturity_years)) - 1]

  dt[, forward_value := forward_dec * 100]
  dt[, segment_label := paste0(prev_maturity_label, "→\n", as.character(maturity_label))]
  dt[]
}

build_forward_plot <- function(forward_dt, title, subtitle, y_label) {
  y_guide_min <- floor(min(forward_dt$forward_value, na.rm = TRUE) * 10) / 10
  y_guide_max <- ceiling(max(forward_dt$forward_value, na.rm = TRUE) * 10) / 10
  y_tick_min <- floor(min(forward_dt$forward_value, na.rm = TRUE) * 5) / 5
  y_tick_max <- ceiling(max(forward_dt$forward_value, na.rm = TRUE) * 5) / 5
  y_breaks <- seq(y_tick_min, y_tick_max, by = 0.2)
  y_guides <- seq(y_guide_min, y_guide_max, by = 0.1)
  segment_levels <- unique(forward_dt$segment_label)

  p <- ggplot(
    forward_dt,
    aes(
      x = factor(segment_label, levels = segment_levels),
      y = forward_value,
      color = curve_label,
      group = curve_label
    )
  ) +
    geom_hline(
      yintercept = y_guides,
      color = "#D9DEE5",
      linewidth = 0.28
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.1) +
    scale_y_continuous(breaks = y_breaks) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Forward segment",
      y = y_label,
      color = NULL
    )

  investlabr::viz_theme_apply(
    p,
    style = "macro_classic",
    context = "report",
    legend_position = "bottom",
    show_compiler = FALSE
  )
}

curve_dates <- select_curve_dates(real_dt)
nominal_curve <- prep_curve_dt(nominal_dt, nominal_tenor_map, curve_dates = curve_dates)
real_curve <- prep_curve_dt(real_dt, real_tenor_map, curve_dates = curve_dates)

p_nominal <- build_curve_plot(
  nominal_curve$data,
  title = "Nominal Treasury curve",
  subtitle = "Par yields across standard maturities.",
  y_label = "Yield (%)"
)

p_real <- build_curve_plot(
  real_curve$data,
  title = "Real Treasury curve",
  subtitle = "Inflation-adjusted yields across standard maturities.",
  y_label = "Real yield (%)"
)

nominal_forward_dt <- build_forward_dt_from_curve(nominal_curve$data)
real_forward_dt <- build_forward_dt_from_curve(real_curve$data)

p_nominal_forward <- build_forward_plot(
  nominal_forward_dt,
  title = "Nominal implied forward curve",
  subtitle = "Adjacent forward segments derived approximately from par yields.",
  y_label = "Implied forward yield (%)"
)

p_real_forward <- build_forward_plot(
  real_forward_dt,
  title = "Real implied forward curve",
  subtitle = "Adjacent forward segments derived approximately from real yields.",
  y_label = "Implied forward real yield (%)"
)

comparison_dates <- unique(c(nominal_curve$curve_dates, real_curve$curve_dates))
comparison_dates <- sort(as.Date(comparison_dates))

plot <- investlabr::gen_grid_of_plots_with_labels(
  plots = list(
    p_nominal, p_real,
    p_nominal_forward, p_real_forward
  ),
  n_rows = 2,
  n_cols = 2,
  title = "Nominal, Real, and Implied Forward Treasury Curves",
  bottom = paste(
    "Source: U.S. Department of the Treasury.",
    paste0(
      "The three comparison dates are the latest available curve date, the date with the highest real 10-year yield in the last three months, and the date with the lowest real 10-year yield in the last three months.",
      " Forward panels show adjacent implied forward segments computed approximately from reported yields rather than exact bootstrapped zero-coupon forwards.",
      " In this run those dates are ",
      paste(format(comparison_dates, "%Y-%m-%d"), collapse = " and "),
      "."
    )
  ),
  style = "macro_classic",
  context = "report",
  show_compiler = TRUE
)

print(plot)
