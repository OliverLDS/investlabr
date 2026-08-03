#' Plot a forward percentile fan
#'
#' @param fan_dt Data frame containing code{horizon}, code{p10}, code{p25},
#'   code{p50}, code{p75}, and code{p90}.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param y_label Y-axis label.
#' @param accent Optional fan color.
#' @param show_compiler Whether to append the configured compiler footer.
#' @inheritParams viz_style_get
#'
#' @return A ggplot object.
#' @export
viz_forward_fan <- function(
  fan_dt,
  title,
  subtitle,
  y_label,
  accent = NULL,
  style = NULL,
  context = NULL,
  show_compiler = TRUE
) {
  required <- c("horizon", "p10", "p25", "p50", "p75", "p90")
  missing <- setdiff(required, names(fan_dt))
  if (length(missing) > 0L) {
    stop("`fan_dt` is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  resolved <- .viz_resolve_style(style = style, context = context)
  if (is.null(accent)) accent <- resolved$accent
  p <- ggplot2::ggplot(fan_dt, ggplot2::aes(x = .data[["horizon"]])) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[["p10"]], ymax = .data[["p90"]]),
      fill = accent,
      alpha = 0.16
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[["p25"]], ymax = .data[["p75"]]),
      fill = accent,
      alpha = 0.28
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data[["p50"]]), color = accent, linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = .data[["p50"]]), color = accent, size = 1.8) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Forward business days", y = y_label)
  viz_theme_apply(
    p,
    style = resolved,
    legend_position = "none",
    show_compiler = show_compiler
  )
}

#' Plot one or more research series
#'
#' @param data Data frame containing the plotted columns.
#' @param x,y,color Column names mapped to x, y, and color.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param y_label Y-axis label.
#' @param colors Optional named manual color vector.
#' @param legend_position Legend position.
#' @param show_compiler Whether to append the configured compiler footer.
#' @inheritParams viz_style_get
#'
#' @return A ggplot object.
#' @export
viz_series_lines <- function(
  data,
  x,
  y,
  color = "series",
  title,
  subtitle,
  y_label,
  colors = NULL,
  style = NULL,
  context = NULL,
  legend_position = "bottom",
  show_compiler = TRUE
) {
  required <- c(x, y, color)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("`data` is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color]])
  ) +
    ggplot2::geom_line(linewidth = 0.85, na.rm = TRUE) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = y_label, color = NULL)
  if (!is.null(colors)) p <- p + ggplot2::scale_color_manual(values = colors)
  viz_theme_apply(
    p,
    style = style,
    context = context,
    legend_position = legend_position,
    show_compiler = show_compiler
  )
}
