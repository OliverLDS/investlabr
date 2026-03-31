#' Scatter: annual return vs max drawdown
#'
#' @param bt_res_list data.table of backtest rows (from \code{eval_strat_performance()}).
#' @param plot_title string.
#' @param opt_portfolio_line logical; unused placeholder.
#' @return ggplot object.
#' @export
eval_strat_plot_scatter_maxdd_annret <- function(bt_res_list, plot_title = "", opt_portfolio_line = FALSE) {
  bt_res_list |>
    ggplot2::ggplot(ggplot2::aes(x = max_drawdown, y = annual_return, color = asset_name)) +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(ggplot2::aes(label = sprintf("%s_%s", substr(asset_name, 1, 3), strat_label)), vjust = -0.5, size = 3) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(x = "MoD", y = "Ret", title = plot_title) +
    ggplot2::theme_minimal()
}

#' Plot equity curve (time series)
#'
#' @param bt_res single-row data.table from \code{eval_strat_performance()}.
#' @return ggplot object.
#' @export
eval_strat_plot_tsline_eq <- function(bt_res, benchmark_bt_res = NULL) {
  bt_dt <- bt_res$log_ret_dt[[1]]
  data.table::set(bt_dt, j = "eq", value = .log_ret_to_equity(bt_dt$log_ret))
  p <- bt_dt |>
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = eq)) +
    ggplot2::geom_line(color = "#E69F00")
  title <- sprintf("%s | %s", bt_res$asset_name, bt_res$strat_label)
  subtitle <- sprintf("%s ~ %s", format(bt_res$start, "%Y-%m-%d"), format(bt_res$end, "%Y-%m-%d"))
  caption <- sprintf("Evaluated | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(bt_res$total_return * 100, 2), round(bt_res$annual_return * 100, 2), round(bt_res$max_drawdown * 100, 2))
  if (!is.null(benchmark_bt_res)) {
    benchmark_bt_dt <- benchmark_bt_res$log_ret_dt[[1]]
    data.table::set(benchmark_bt_dt, j = "eq", value = .log_ret_to_equity(benchmark_bt_dt$log_ret))
    p <- p + ggplot2::geom_line(data = benchmark_bt_dt, ggplot2::aes(x = datetime, y = eq), color = "#7F7F7F")
    caption <- paste0(caption, sprintf("\nBenchmark | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%", round(benchmark_bt_res$total_return * 100, 2), round(benchmark_bt_res$annual_return * 100, 2), round(benchmark_bt_res$max_drawdown * 100, 2)))
  }
  p <- p + ggplot2::labs(
    x = "", y = "",
    title = title, subtitle = subtitle, caption = caption
  ) +
    ggplot2::theme_minimal()
  p
}
