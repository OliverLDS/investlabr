#' Scatter: annual return vs max drawdown
#'
#' @param bt_res_list data.table of backtest rows (from \code{eval_strat_performance()}).
#' @param plot_title string.
#' @param opt_portfolio_line logical; unused placeholder.
#' @inheritParams viz_style_get
#' @return ggplot object.
#' @export
eval_strat_plot_scatter_maxdd_annret <- function(bt_res_list, plot_title = "", opt_portfolio_line = FALSE, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  p <- bt_res_list |>
    ggplot2::ggplot(ggplot2::aes(x = max_drawdown, y = annual_return, color = asset_name)) +
    ggplot2::geom_point(size = resolved$point_size * 1.2) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = sprintf("%s_%s", substr(asset_name, 1, 3), strat_label)),
      vjust = -0.5,
      size = resolved$label_size,
      color = resolved$ink
    ) +
    ggplot2::scale_color_manual(values = rep_len(resolved$discrete, length(unique(bt_res_list$asset_name)))) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(x = "MoD", y = "Ret", title = plot_title) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
  viz_theme_apply(p, style = resolved, legend_position = "none")
}

#' Plot equity curve (time series)
#'
#' @param bt_res single-row data.table from \code{eval_strat_performance()}.
#' @inheritParams viz_style_get
#' @return ggplot object.
#' @export
eval_strat_plot_tsline_eq <- function(bt_res, benchmark_bt_res = NULL, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  bt_dt <- bt_res$log_ret_dt[[1]]
  data.table::set(bt_dt, j = "eq", value = .log_ret_to_equity(bt_dt$log_ret))
  bt_dt[, series := "Evaluated"]
  plot_dt <- data.table::copy(bt_dt)
  title <- sprintf("%s | %s", bt_res$asset_name, bt_res$strat_label)
  subtitle <- sprintf("%s ~ %s", format(bt_res$start, "%Y-%m-%d"), format(bt_res$end, "%Y-%m-%d"))
  metric_labels <- c(
    Evaluated = sprintf("Evaluated | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(bt_res$total_return * 100, 2), round(bt_res$annual_return * 100, 2), round(bt_res$max_drawdown * 100, 2))
  )
  if (!is.null(benchmark_bt_res)) {
    benchmark_bt_dt <- benchmark_bt_res$log_ret_dt[[1]]
    data.table::set(benchmark_bt_dt, j = "eq", value = .log_ret_to_equity(benchmark_bt_dt$log_ret))
    benchmark_bt_dt[, series := "Benchmark"]
    plot_dt <- data.table::rbindlist(list(plot_dt, benchmark_bt_dt), use.names = TRUE, fill = TRUE)
    metric_labels <- c(
      metric_labels,
      Benchmark = sprintf("Benchmark | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(benchmark_bt_res$total_return * 100, 2), round(benchmark_bt_res$annual_return * 100, 2), round(benchmark_bt_res$max_drawdown * 100, 2))
    )
  }
  series_colors <- c(Evaluated = resolved$accent, Benchmark = resolved$muted)
  series_levels <- names(metric_labels)
  plot_dt[, series := factor(series, levels = series_levels)]
  key_dt <- .equity_metric_key_dt(plot_dt, metric_labels, series_colors)

  p <- plot_dt |>
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = eq, color = series)) +
    ggplot2::geom_line(linewidth = resolved$line_width) +
    ggplot2::geom_segment(
      data = key_dt,
      ggplot2::aes(x = x, xend = xend, y = y, yend = y, color = series),
      inherit.aes = FALSE,
      linewidth = resolved$line_width * 1.3,
      lineend = "round"
    ) +
    ggplot2::geom_text(
      data = key_dt,
      ggplot2::aes(x = label_x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0.5,
      color = resolved$muted,
      size = resolved$caption_size / 3.1
    ) +
    ggplot2::scale_color_manual(values = series_colors[series_levels], breaks = series_levels)
  p <- p + ggplot2::labs(
    x = "", y = "",
    title = title, subtitle = subtitle,
    color = NULL
  ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01),
      limits = c(min(key_dt$y, na.rm = TRUE) - diff(range(plot_dt$eq, na.rm = TRUE)) * 0.03, max(plot_dt$eq, na.rm = TRUE))
    )
  p <- viz_theme_apply(p, style = resolved, legend_position = "none")
  p <- viz_annotate_last_value(p, bt_dt, x = "datetime", y = "eq", style = resolved, digits = 2, color = resolved$accent)
  if (!is.null(benchmark_bt_res)) {
    p <- viz_annotate_last_value(p, benchmark_bt_dt, x = "datetime", y = "eq", style = resolved, digits = 2, color = resolved$muted)
  }
  p
}

.equity_metric_key_dt <- function(plot_dt, metric_labels, series_colors) {
  x_min <- min(plot_dt$datetime, na.rm = TRUE)
  x_max <- max(plot_dt$datetime, na.rm = TRUE)
  x_span <- as.numeric(difftime(x_max, x_min, units = "secs"))
  if (!is.finite(x_span) || x_span <= 0) x_span <- 1

  y_range <- range(plot_dt$eq, na.rm = TRUE)
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span <= 0) y_span <- max(abs(y_range), 1) * 0.05

  n <- length(metric_labels)
  data.table::data.table(
    series = factor(names(metric_labels), levels = names(metric_labels)),
    x = x_min + x_span * 0.02,
    xend = x_min + x_span * 0.055,
    label_x = x_min + x_span * 0.065,
    y = y_range[1] - y_span * (0.10 + 0.06 * (seq_len(n) - 1L)),
    label = unname(metric_labels),
    color = unname(series_colors[names(metric_labels)])
  )
}
