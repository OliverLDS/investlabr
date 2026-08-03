#' Simulate a forward percentile fan from historical changes
#'
#' @param start_value Current level from which paths begin.
#' @param changes Historical one-period changes sampled with replacement.
#' @param horizons Positive integer horizons included in the returned fan.
#' @param n_paths Number of simulated paths.
#' @param seed Optional reproducibility seed.
#'
#' @return A data.table containing p10, p25, p50, p75, and p90 levels by
#'   horizon, including horizon zero.
#' @export
sim_forward_fan <- function(
  start_value,
  changes,
  horizons = c(1L, 5L, 10L, 20L, 60L),
  n_paths = 600L,
  seed = 1L
) {
  changes <- changes[is.finite(changes)]
  horizons <- sort(unique(as.integer(horizons)))
  n_paths <- as.integer(n_paths)
  if (!is.numeric(start_value) || length(start_value) != 1L || !is.finite(start_value)) {
    stop("`start_value` must be one finite number.", call. = FALSE)
  }
  if (length(changes) < 20L) {
    stop("Not enough historical changes to build a fan.", call. = FALSE)
  }
  if (length(horizons) == 0L || anyNA(horizons) || any(horizons < 1L)) {
    stop("`horizons` must contain positive integers.", call. = FALSE)
  }
  if (length(n_paths) != 1L || is.na(n_paths) || n_paths < 1L) {
    stop("`n_paths` must be a positive integer.", call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)
  max_h <- max(horizons)
  sampled <- matrix(
    sample(changes, size = max_h * n_paths, replace = TRUE),
    nrow = max_h,
    ncol = n_paths
  )
  path_levels <- matrix(
    apply(sampled, 2L, cumsum),
    nrow = max_h,
    ncol = n_paths
  ) + start_value
  probs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  fan_values <- t(vapply(
    horizons,
    function(h) stats::quantile(path_levels[h, ], probs = probs, na.rm = TRUE, names = FALSE),
    numeric(length(probs))
  ))
  fan <- data.table::data.table(
    horizon = horizons,
    p10 = fan_values[, 1L],
    p25 = fan_values[, 2L],
    p50 = fan_values[, 3L],
    p75 = fan_values[, 4L],
    p90 = fan_values[, 5L]
  )
  start <- data.table::data.table(
    horizon = 0L,
    p10 = start_value,
    p25 = start_value,
    p50 = start_value,
    p75 = start_value,
    p90 = start_value
  )
  data.table::rbindlist(list(start, fan), use.names = TRUE)
}

#' Construct deterministic scenario paths
#'
#' @param start_value Current level.
#' @param scenario_changes Named numeric vector or list of per-period changes.
#' @param horizon_months Number of forward monthly steps.
#' @param start_date First scenario date.
#'
#' @return Long-form data.table with code{date}, code{scenario}, and
#'   code{value} columns.
#' @export
sim_scenario_path <- function(
  start_value,
  scenario_changes,
  horizon_months = 6L,
  start_date = Sys.Date()
) {
  horizon_months <- as.integer(horizon_months)
  if (!is.numeric(start_value) || length(start_value) != 1L || !is.finite(start_value)) {
    stop("`start_value` must be one finite number.", call. = FALSE)
  }
  if (is.null(names(scenario_changes)) || any(!nzchar(names(scenario_changes)))) {
    stop("`scenario_changes` must be named.", call. = FALSE)
  }
  if (length(horizon_months) != 1L || is.na(horizon_months) || horizon_months < 1L) {
    stop("`horizon_months` must be a positive integer.", call. = FALSE)
  }

  dates <- seq(as.Date(start_date), by = "month", length.out = horizon_months + 1L)
  data.table::rbindlist(lapply(names(scenario_changes), function(scenario) {
    change <- as.numeric(scenario_changes[[scenario]])
    if (length(change) != 1L || !is.finite(change)) {
      stop("Each scenario change must be one finite number.", call. = FALSE)
    }
    data.table::data.table(
      date = dates,
      scenario = scenario,
      value = start_value + c(0, cumsum(rep(change, horizon_months)))
    )
  }))
}
