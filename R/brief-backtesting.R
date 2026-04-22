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
#' @param benchmark_bt_res Optional benchmark strategy result to compare against
#'   the evaluated strategy.
#' @param bt_res_2 Optional second evaluated strategy result. Use sparingly; the
#'   chart is designed for at most two evaluated strategies plus one benchmark.
#' @param caption Optional chart caption placed above the configured compiler footer.
#' @inheritParams viz_style_get
#' @return ggplot object.
#' @export
eval_strat_plot_tsline_eq <- function(bt_res, benchmark_bt_res = NULL, bt_res_2 = NULL, style = NULL, context = NULL, caption = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  bt_dt <- bt_res$log_ret_dt[[1]]
  data.table::set(bt_dt, j = "eq", value = .log_ret_to_equity(bt_dt$log_ret))
  bt_dt[, series := "Evaluated 1"]
  plot_dt <- data.table::copy(bt_dt)
  series_colors <- c("Evaluated 1" = resolved$accent, "Evaluated 2" = resolved$accent2, Benchmark = resolved$muted)
  title <- .equity_plot_title(bt_res, bt_res_2, benchmark_bt_res, series_colors = series_colors)
  subtitle <- .equity_plot_subtitle(bt_res, bt_res_2, benchmark_bt_res)
  metric_labels <- c("Evaluated 1" = .equity_metric_label(bt_res, "Evaluated 1"))
  if (!is.null(bt_res_2)) {
    bt_dt_2 <- bt_res_2$log_ret_dt[[1]]
    data.table::set(bt_dt_2, j = "eq", value = .log_ret_to_equity(bt_dt_2$log_ret))
    bt_dt_2[, series := "Evaluated 2"]
    plot_dt <- data.table::rbindlist(list(plot_dt, bt_dt_2), use.names = TRUE, fill = TRUE)
    metric_labels <- c(
      metric_labels,
      "Evaluated 2" = .equity_metric_label(bt_res_2, "Evaluated 2")
    )
  }
  if (!is.null(benchmark_bt_res)) {
    benchmark_bt_dt <- benchmark_bt_res$log_ret_dt[[1]]
    data.table::set(benchmark_bt_dt, j = "eq", value = .log_ret_to_equity(benchmark_bt_dt$log_ret))
    benchmark_bt_dt[, series := "Benchmark"]
    plot_dt <- data.table::rbindlist(list(plot_dt, benchmark_bt_dt), use.names = TRUE, fill = TRUE)
    metric_labels <- c(
      metric_labels,
      Benchmark = .equity_metric_label(benchmark_bt_res, "Benchmark")
    )
  }
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
    color = NULL,
    caption = caption
  ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01),
      limits = c(min(key_dt$y, na.rm = TRUE) - diff(range(plot_dt$eq, na.rm = TRUE)) * 0.03, max(plot_dt$eq, na.rm = TRUE))
    )
  p <- viz_theme_apply(p, style = resolved, legend_position = "none")
  p <- viz_annotate_last_value(p, bt_dt, x = "datetime", y = "eq", style = resolved, digits = 2, color = resolved$accent)
  if (!is.null(bt_res_2)) {
    p <- viz_annotate_last_value(p, bt_dt_2, x = "datetime", y = "eq", style = resolved, digits = 2, color = resolved$accent2)
  }
  if (!is.null(benchmark_bt_res)) {
    p <- viz_annotate_last_value(p, benchmark_bt_dt, x = "datetime", y = "eq", style = resolved, digits = 2, color = resolved$muted)
  }
  p
}

.equity_plot_title <- function(bt_res, bt_res_2 = NULL, benchmark_bt_res = NULL, series_colors = NULL) {
  lines <- c(
    sprintf(
      "Evaluated 1 (%s): %s | %s",
      .equity_color_name(series_colors[["Evaluated 1"]]),
      bt_res$asset_name,
      bt_res$strat_label
    )
  )
  if (!is.null(bt_res_2)) {
    lines <- c(
      lines,
      sprintf(
        "Evaluated 2 (%s): %s | %s",
        .equity_color_name(series_colors[["Evaluated 2"]]),
        bt_res_2$asset_name,
        bt_res_2$strat_label
      )
    )
  }
  if (!is.null(benchmark_bt_res)) {
    lines <- c(
      lines,
      sprintf(
        "Benchmark (%s): %s | %s",
        .equity_color_name(series_colors[["Benchmark"]]),
        benchmark_bt_res$asset_name,
        benchmark_bt_res$strat_label
      )
    )
  }
  paste(lines, collapse = "\n")
}

.equity_color_name <- function(hex) {
  color_names <- c(
    "#0F4C5C" = "deep teal",
    "#C97A2B" = "copper orange",
    "#6B7280" = "cool gray",
    "#AA3A2A" = "burnt red",
    "#214E63" = "deep steel blue",
    "#736B5E" = "warm taupe gray",
    "#2EC4B6" = "bright teal",
    "#FFB703" = "amber",
    "#94A3B8" = "blue gray",
    "#1D3557" = "navy blue",
    "#E76F51" = "coral",
    "#404040" = "charcoal gray",
    "#7A7A7A" = "neutral gray",
    "#6B6B6B" = "medium gray",
    "#C1121F" = "crimson red",
    "#003049" = "deep navy",
    "#6B705C" = "olive gray",
    "#7D3C2E" = "mahogany",
    "#355C7D" = "steel blue",
    "#756B5B" = "taupe gray",
    "#163A5F" = "institutional navy",
    "#6E8CA6" = "muted steel blue",
    "#627286" = "slate blue gray",
    "#567A6E" = "sage green",
    "#B07D4F" = "warm tan",
    "#687166" = "moss gray",
    "#4CC9F0" = "cyan blue",
    "#F4A261" = "sand orange",
    "#93A4B3" = "pale blue gray",
    "#C44536" = "brick red",
    "#1F4E79" = "deep blue",
    "#6E6A61" = "warm gray",
    "#2B2B2B" = "near black",
    "#666666" = "medium gray",
    "#5C5C5C" = "slate gray"
  )
  name <- unname(color_names[toupper(hex)])
  ifelse(is.na(name), "custom color", name)
}

.equity_plot_subtitle <- function(bt_res, bt_res_2 = NULL, benchmark_bt_res = NULL) {
  .equity_warn_if_period_differs(bt_res, bt_res_2, "Evaluated 2")
  .equity_warn_if_period_differs(bt_res, benchmark_bt_res, "Benchmark")
  sprintf("%s ~ %s", format(bt_res$start, "%Y-%m-%d"), format(bt_res$end, "%Y-%m-%d"))
}

.equity_warn_if_period_differs <- function(reference_bt_res, candidate_bt_res, candidate_name) {
  if (is.null(candidate_bt_res)) {
    return(invisible(NULL))
  }
  same_start <- identical(as.Date(reference_bt_res$start), as.Date(candidate_bt_res$start))
  same_end <- identical(as.Date(reference_bt_res$end), as.Date(candidate_bt_res$end))
  if (!same_start || !same_end) {
    warning(
      sprintf(
        "%s uses a different backtest period from Evaluated 1; the plot subtitle shows the Evaluated 1 period.",
        candidate_name
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

.equity_metric_label <- function(bt_res, prefix) {
  label <- sprintf(
    "%s | total_ret: %s%%; ann_ret: %s%%; max_dd: %s%%",
    prefix,
    round(bt_res$total_return * 100, 2),
    round(bt_res$annual_return * 100, 2),
    round(bt_res$max_drawdown * 100, 2)
  )
  if ("annual_volatility" %in% names(bt_res)) {
    label <- paste0(label, sprintf("; vol: %s%%", round(bt_res$annual_volatility * 100, 2)))
  }
  if ("sharpe_ratio" %in% names(bt_res)) {
    label <- paste0(label, sprintf("; Sharpe: %s", round(bt_res$sharpe_ratio, 2)))
  }
  if ("sortino_ratio" %in% names(bt_res)) {
    label <- paste0(label, sprintf("; Sortino: %s", round(bt_res$sortino_ratio, 2)))
  }
  label
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
