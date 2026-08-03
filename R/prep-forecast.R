#' Carry the latest observed value forward
#'
#' @param x Vector containing values and missing observations.
#'
#' @return A vector with missing values filled from the latest prior observation.
#' @export
prep_fill_forward <- function(x) {
  if (length(x) == 0L) return(x)
  out <- x
  last <- NA
  for (i in seq_along(out)) {
    if (!is.na(out[i])) {
      last <- out[i]
    } else if (!is.na(last)) {
      out[i] <- last
    }
  }
  out
}

#' Combine labeled time series into a research-ready wide table
#'
#' @param series_list List of data frames or data.tables containing code{date},
#'   code{value}, and code{series} columns.
#' @param fill_forward Whether to carry each series forward after reshaping.
#'
#' @return A data.table with one row per date and one column per series.
#' @export
prep_series_wide <- function(series_list, fill_forward = TRUE) {
  if (!is.list(series_list) || length(series_list) == 0L) {
    stop("`series_list` must be a non-empty list.", call. = FALSE)
  }
  long <- data.table::rbindlist(series_list, fill = TRUE)
  required <- c("date", "value", "series")
  missing <- setdiff(required, names(long))
  if (length(missing) > 0L) {
    stop("Series data are missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- data.table::dcast(long, date ~ series, value.var = "value")
  data.table::setorder(out, date)
  value_cols <- setdiff(names(out), "date")
  if (isTRUE(fill_forward) && length(value_cols) > 0L) {
    out[, (value_cols) := lapply(.SD, prep_fill_forward), .SDcols = value_cols]
  }
  out
}

#' Standardize a numeric series
#'
#' @param x Numeric vector.
#'
#' @return Numeric z-scores. Returns all code{NA} when dispersion is zero or
#'   unavailable.
#' @export
prep_zscore <- function(x) {
  sdv <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / sdv
}

#' Extract recent finite first differences
#'
#' @param x Numeric level series.
#' @param lookback Maximum number of recent changes to return.
#'
#' @return Numeric vector of recent finite first differences.
#' @export
prep_recent_changes <- function(x, lookback = 252L) {
  lookback <- as.integer(lookback)
  if (length(lookback) != 1L || is.na(lookback) || lookback < 1L) {
    stop("`lookback` must be a positive integer.", call. = FALSE)
  }
  x <- x[is.finite(x)]
  dx <- diff(x)
  utils::tail(dx[is.finite(dx)], lookback)
}
