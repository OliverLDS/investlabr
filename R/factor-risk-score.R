#' Map standardized pressure into a bounded research score
#'
#' @param z Numeric standardized signal.
#' @param center Logistic midpoint.
#' @param scale Positive logistic scale.
#'
#' @return Numeric score bounded between 0 and 100.
#' @export
factor_bounded_score <- function(z, center = 0, scale = 1.2) {
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale) || scale <= 0) {
    stop("`scale` must be one positive finite number.", call. = FALSE)
  }
  100 / (1 + exp(-(z - center) / scale))
}
