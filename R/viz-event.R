#' @export
gen_plot_event_tsline_cum_ret <- function(res, threshold = 0.02) {
  wide_DT <- res$performance_by_horizons[[1]][, c("Horizon", "10%", "25%", "50%", "75%", "90%")]
  long_DT <- data.table::melt(wide_DT, id.vars = "Horizon", variable.name = "stat", value.name = "value")
  ggplot2::ggplot(long_DT, ggplot2::aes(x = Horizon, y = value, color = stat)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", color = "gray40") +
    ggplot2::geom_hline(yintercept = -threshold, linetype = "dashed", color = "gray40") +
    ggplot2::labs(
      title = "",
      x = "",
      y = "",
      color = NULL
    ) +
    ggplot2::theme_minimal()
}

#' @export
gen_plot_comparing_events <- function(first_event_res, second_event_res, first_event_label, second_event_label, first_event_color = "#2ECC71", second_event_color = "#E74C3C") {
  event_res <- rbind(
    first_event_res$performance_by_horizons[[1]][, .(Horizon, Mean)][, pos := first_event_label],
    second_event_res$performance_by_horizons[[1]][, .(Horizon, Mean)][, pos := second_event_label]
  )
  event_res |>
    ggplot2::ggplot(ggplot2::aes(x = Horizon, y = Mean, group = pos, color = pos)) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::scale_color_manual(
      values = setNames(
        c(first_event_color, second_event_color),
        c(first_event_label, second_event_label)
      )
    ) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}
