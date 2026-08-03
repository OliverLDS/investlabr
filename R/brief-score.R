#' Format the latest bounded research score
#'
#' @param score Numeric score on a 0 to 100 scale.
#' @param moderate Minimum score labeled moderate.
#' @param elevated Minimum score labeled elevated.
#'
#' @return A concise reader-facing character label.
#' @export
brief_score_label <- function(score, moderate = 40, elevated = 70) {
  if (length(score) != 1L || !is.finite(score)) return("Latest: unavailable")
  label <- if (score >= elevated) "elevated" else if (score >= moderate) "moderate" else "low"
  paste0("Latest: ", label, " (", round(score), "/100)")
}
