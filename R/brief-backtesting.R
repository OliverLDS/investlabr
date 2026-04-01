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
  p <- bt_dt |>
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = eq)) +
    ggplot2::geom_line(color = resolved$accent, linewidth = resolved$line_width)
  title <- sprintf("%s | %s", bt_res$asset_name, bt_res$strat_label)
  subtitle <- sprintf("%s ~ %s", format(bt_res$start, "%Y-%m-%d"), format(bt_res$end, "%Y-%m-%d"))
  caption <- sprintf("Evaluated | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(bt_res$total_return * 100, 2), round(bt_res$annual_return * 100, 2), round(bt_res$max_drawdown * 100, 2))
  if (!is.null(benchmark_bt_res)) {
    benchmark_bt_dt <- benchmark_bt_res$log_ret_dt[[1]]
    data.table::set(benchmark_bt_dt, j = "eq", value = .log_ret_to_equity(benchmark_bt_dt$log_ret))
    p <- p + ggplot2::geom_line(data = benchmark_bt_dt, ggplot2::aes(x = datetime, y = eq), color = resolved$muted, linewidth = resolved$line_width)
    caption <- paste0(caption, sprintf("\nBenchmark | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(benchmark_bt_res$total_return * 100, 2), round(benchmark_bt_res$annual_return * 100, 2), round(benchmark_bt_res$max_drawdown * 100, 2)))
  }
  p <- p + ggplot2::labs(
    x = "", y = "",
    title = title, subtitle = subtitle, caption = caption
  ) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  p <- viz_theme_apply(p, style = resolved)
  p <- viz_annotate_last_value(p, bt_dt, x = "datetime", y = "eq", style = resolved, digits = 2, color = resolved$accent)
  if (!is.null(benchmark_bt_res)) {
    p <- viz_annotate_last_value(p, benchmark_bt_dt, x = "datetime", y = "eq", style = resolved, digits = 2, color = resolved$muted)
  }
  p
}
