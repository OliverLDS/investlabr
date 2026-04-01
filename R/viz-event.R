#' @inheritParams viz_style_get
#' @export
gen_plot_event_tsline_cum_ret <- function(res, threshold = 0.02, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  wide_DT <- res$performance_by_horizons[[1]][, c("Horizon", "10%", "25%", "50%", "75%", "90%")]
  long_DT <- data.table::melt(wide_DT, id.vars = "Horizon", variable.name = "stat", value.name = "value")
  p <- ggplot2::ggplot(long_DT, ggplot2::aes(x = Horizon, y = value, color = stat)) +
    ggplot2::geom_line(linewidth = resolved$line_width) +
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", color = resolved$muted) +
    ggplot2::geom_hline(yintercept = -threshold, linetype = "dashed", color = resolved$muted) +
    ggplot2::scale_color_manual(values = setNames(rep_len(resolved$discrete, length(unique(long_DT$stat))), unique(long_DT$stat))) +
    ggplot2::labs(
      title = "",
      x = "",
      y = "",
      color = NULL
    )
  viz_theme_apply(p, style = resolved)
}

#' @inheritParams viz_style_get
#' @export
gen_plot_comparing_events <- function(first_event_res, second_event_res, first_event_label, second_event_label, first_event_color = NULL, second_event_color = NULL, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  if (is.null(first_event_color)) first_event_color <- resolved$accent
  if (is.null(second_event_color)) second_event_color <- resolved$accent2
  event_res <- rbind(
    first_event_res$performance_by_horizons[[1]][, .(Horizon, Mean)][, pos := first_event_label],
    second_event_res$performance_by_horizons[[1]][, .(Horizon, Mean)][, pos := second_event_label]
  )
  p <- event_res |>
    ggplot2::ggplot(ggplot2::aes(x = Horizon, y = Mean, group = pos, color = pos)) +
    ggplot2::geom_line(linewidth = resolved$line_width * 1.4) +
    ggplot2::scale_color_manual(
      values = setNames(
        c(first_event_color, second_event_color),
        c(first_event_label, second_event_label)
      )
    ) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(legend.position = "none")
  label_dt <- event_res[, .SD[.N], by = pos]
  p <- p +
    ggplot2::geom_text(
      data = label_dt,
      ggplot2::aes(x = Horizon, y = Mean, label = pos, color = pos),
      hjust = -0.1,
      size = resolved$label_size,
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(clip = "off")
  viz_theme_apply(p, style = resolved, legend_position = "none")
}
