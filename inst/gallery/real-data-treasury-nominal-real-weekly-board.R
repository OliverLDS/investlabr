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
  y_min <- floor(min(curve_dt$value, na.rm = TRUE) * 10) / 10
  y_max <- ceiling(max(curve_dt$value, na.rm = TRUE) * 10) / 10
  y_breaks <- seq(y_min, y_max, by = 0.1)
  y_minor_breaks <- seq(y_min, y_max, by = 0.01)
  y_minor_breaks <- setdiff(round(y_minor_breaks, 8), round(y_breaks, 8))

  p <- ggplot(
    curve_dt,
    aes(x = maturity_label, y = value, color = curve_label, group = curve_label)
  ) +
    geom_hline(
      yintercept = y_minor_breaks,
      color = "#D9DEE5",
      linewidth = 0.28
    ) +
    geom_hline(
      yintercept = y_breaks,
      color = "#D7DCE2",
      linewidth = 0.3
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

comparison_dates <- unique(c(nominal_curve$curve_dates, real_curve$curve_dates))
comparison_dates <- sort(as.Date(comparison_dates))

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_nominal, p_real),
  n_rows = 1,
  n_cols = 2,
  title = "Nominal and Real Treasury Curves at Recent Real-Rate Extremes",
  bottom = paste(
    "Source: U.S. Department of the Treasury.",
    paste0(
      "The three comparison dates are the latest available curve date, the date with the highest real 10-year yield in the last three months, and the date with the lowest real 10-year yield in the last three months.",
      " In this run those dates are ",
      paste(format(comparison_dates, "%Y-%m-%d"), collapse = " and "),
      "."
    )
  ),
  style = "macro_classic",
  context = "report",
  show_compiler = TRUE
)
