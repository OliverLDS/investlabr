#' Generate a base candlestick plot
#'
#' @param DT A data.table containing OHLC data with columns \code{datetime},
#'   \code{open}, \code{high}, \code{low}, and \code{close}.
#' @inheritParams viz_style_get
#'
#' @return A ggplot2 object showing an OHLC candlestick chart.
#' @export
viz_candle_base <- function(DT, style = NULL, context = NULL, show_compiler = TRUE) {
  resolved <- .viz_resolve_style(style = style, context = context)
  p <- .viz_candle_core_plot(DT, style = resolved) +
    ggplot2::labs(x = "", y = "")
  viz_theme_apply(p, style = resolved, legend_position = "none", show_compiler = show_compiler)
}

.viz_candle_core_plot <- function(DT, style) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("datetime", "open", "high", "low", "close") %in% names(DT)))

  plot_dt <- data.table::copy(DT)
  plot_dt[, candle_color := ifelse(open < close, style$up, style$down)]
  step <- if (nrow(plot_dt) > 1L) {
    stats::median(diff(as.numeric(plot_dt$datetime)), na.rm = TRUE)
  } else {
    1
  }
  if (!is.finite(step) || step <= 0) {
    step <- 1
  }
  body_half <- 0.45 * step
  wick_half <- body_half * 0.15

  ggplot2::ggplot(plot_dt, ggplot2::aes(x = datetime)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = datetime - wick_half,
        xmax = datetime + wick_half,
        ymin = low,
        ymax = high
      ),
      fill = plot_dt$candle_color,
      color = NA
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = datetime - body_half,
        xmax = datetime + body_half,
        ymin = pmin(open, close),
        ymax = pmax(open, close)
      ),
      fill = plot_dt$candle_color,
      color = NA
    )
}

#' Generate candlestick plot with support and resistance lines
#'
#' @param DT A data.table containing OHLC data with columns \code{datetime}, \code{open}, \code{high}, \code{low}, and \code{close}.
#' @param support_pts Numeric vector of support price levels.
#' @param resistance_pts Numeric vector of resistance price levels.
#' @inheritParams viz_style_get
#'
#' @return A ggplot2 object showing candlestick chart with support and resistance lines.
#' @export
gen_candle_plots_with_sr_lines <- function(DT, support_pts, resistance_pts, near_frac = 0.01, label_digits = 2, show_ema_lines = FALSE, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)

  line_dt <- data.table::data.table(
    y = c(support_pts, resistance_pts),
    kind = c(
      rep("support", length(support_pts)),
      rep("resistance", length(resistance_pts))
    )
  )

  if (nrow(line_dt) > 0L) {
    line_dt[, color := ifelse(kind == "support", resolved$support, resolved$resistance)]
    line_dt <- line_dt[order(y)]

    rng_diff <- max(DT$high) - min(DT$low)
    base_step <- rng_diff * 0.01
    thresh <- rng_diff * near_frac

    line_dt[, y_offset := +base_step]

    if (nrow(line_dt) > 1L) {
      for (i in 2:nrow(line_dt)) {
        if (abs(line_dt$y[i] - line_dt$y[i - 1L]) < thresh) {
          if (line_dt$y[i] > line_dt$y[i - 1L]) {
            line_dt$y_offset[i] <- +base_step
            line_dt$y_offset[i - 1L] <- -base_step
          } else {
            line_dt$y_offset[i] <- -base_step
            line_dt$y_offset[i - 1L] <- +base_step
          }
        }
      }
    }
  } else {
    line_dt <- NULL
  }

  x_min <- min(DT$datetime)
  x_max <- max(DT$datetime)
  x_lab <- x_max

  p <- .viz_candle_core_plot(DT, style = resolved)

  if (show_ema_lines) {
    stopifnot(all(c("ema_20", "ema_50", "ema_100", "ema_200") %in% names(DT)))

    ema_specs <- .viz_market_ema_specs(style = resolved)
    ema_cols <- ema_specs$colors
    ema_alpha <- ema_specs$alpha

    ema_label_dt <- data.table::data.table(
      x_lab_ema = min(DT$datetime),
      ema = c("ema_20", "ema_50", "ema_100", "ema_200"),
      y = as.numeric(DT[1, .(ema_20, ema_50, ema_100, ema_200)]),
      label = c("EMA20", "EMA50", "EMA100", "EMA200"),
      color = ema_cols[c("ema_20", "ema_50", "ema_100", "ema_200")],
      alpha = ema_alpha[c("ema_20", "ema_50", "ema_100", "ema_200")]
    )

    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = ema_20), color = ema_cols["ema_20"], alpha = ema_alpha["ema_20"], linewidth = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = ema_50), color = ema_cols["ema_50"], alpha = ema_alpha["ema_50"], linewidth = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = ema_100), color = ema_cols["ema_100"], alpha = ema_alpha["ema_100"], linewidth = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = ema_200), color = ema_cols["ema_200"], alpha = ema_alpha["ema_200"], linewidth = 0.6)

    p <- p + ggplot2::geom_text(
      data = ema_label_dt,
      ggplot2::aes(
        x = x_lab_ema,
        y = y,
        label = label,
        color = color,
        alpha = alpha
      ),
      hjust = -0.1,
      size = 3,
      show.legend = FALSE
    )
  }

  if (!is.null(line_dt) && nrow(line_dt) > 0L) {
    p <- p +
      ggplot2::geom_hline(
        data = line_dt[kind == "support"],
        ggplot2::aes(yintercept = y),
        color = resolved$support
      ) +
      ggplot2::geom_hline(
        data = line_dt[kind == "resistance"],
        ggplot2::aes(yintercept = y),
        color = resolved$resistance
      ) +
      ggplot2::geom_text(
        data = line_dt,
        ggplot2::aes(
          x = x_lab,
          y = y + y_offset,
          label = formatC(round(y, label_digits), format = "f", big.mark = ",", digits = label_digits),
          color = color
        ),
        hjust = -0.1,
        size = resolved$label_size,
        show.legend = FALSE
      )
  }

  p <- p +
    ggplot2::scale_color_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(
      xlim = c(x_min, x_max + (x_max - x_min) * 0.05),
      clip = "off"
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 40, 5.5, 5.5))
  p <- viz_theme_apply(p, style = resolved, legend_position = "none")
  viz_annotate_last_value(p, DT, x = "datetime", y = "close", prefix = "Last: ", digits = label_digits, color = resolved$ink, style = resolved)
}

.gen_candle_plot <- function(DT) {
  resolved <- .viz_resolve_style()
  .viz_candle_core_plot(DT, style = resolved)
}

#' @inheritParams viz_style_get
#' @export
gen_candle_plots_with_sr_dts <- function(
  DT,
  inst_id,
  bar,
  support_dt = NULL,
  resistance_dt = NULL,
  near_frac = 0.01,
  label_digits = 2,
  show_ema_lines = FALSE,
  style = NULL,
  context = NULL,
  show_compiler = TRUE
) {
  resolved <- .viz_resolve_style(style = style, context = context)
  stopifnot(!is.null(support_dt) && !is.null(resistance_dt))
  stopifnot(nrow(support_dt) > 0L && nrow(resistance_dt) > 0L)

  line_dt <- rbind(support_dt[, kind := "support"], resistance_dt[, kind := "resistance"])
  line_dt[, color := ifelse(kind == "support", resolved$support, resolved$resistance)]
  line_dt <- line_dt[order(zone_center)]

  rng_diff <- max(DT$high) - min(DT$low)
  base_step <- rng_diff * 0.01
  thresh <- rng_diff * near_frac

  line_dt[, y_offset := +base_step]
  for (i in 2:nrow(line_dt)) {
    if (abs(line_dt$zone_center[i] - line_dt$zone_center[i - 1L]) < thresh) {
      if (line_dt$zone_center[i] > line_dt$zone_center[i - 1L]) {
        line_dt$y_offset[i] <- +base_step
        line_dt$y_offset[i - 1L] <- -base_step
      } else {
        line_dt$y_offset[i] <- -base_step
        line_dt$y_offset[i - 1L] <- +base_step
      }
    }
  }

  x_min <- min(DT$datetime)
  x_max <- max(DT$datetime)
  x_lab <- x_max

  p_main <- .viz_candle_core_plot(DT, style = resolved)

  if (show_ema_lines) {
    stopifnot(all(c("ema_20", "ema_50", "ema_100", "ema_200") %in% names(DT)))

    ema_specs <- .viz_market_ema_specs(style = resolved)
    ema_cols <- ema_specs$colors
    ema_alpha <- ema_specs$alpha

    ema_label_dt <- data.table::data.table(
      x_lab_ema = min(DT$datetime),
      ema = c("ema_20", "ema_50", "ema_100", "ema_200"),
      y = as.numeric(DT[1, .(ema_20, ema_50, ema_100, ema_200)]),
      label = c("EMA20", "EMA50", "EMA100", "EMA200"),
      color = ema_cols[c("ema_20", "ema_50", "ema_100", "ema_200")],
      alpha = ema_alpha[c("ema_20", "ema_50", "ema_100", "ema_200")]
    )

    p_main <- p_main +
      ggplot2::geom_line(ggplot2::aes(y = ema_20), color = ema_cols["ema_20"], alpha = ema_alpha["ema_20"], linewidth = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = ema_50), color = ema_cols["ema_50"], alpha = ema_alpha["ema_50"], linewidth = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = ema_100), color = ema_cols["ema_100"], alpha = ema_alpha["ema_100"], linewidth = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = ema_200), color = ema_cols["ema_200"], alpha = ema_alpha["ema_200"], linewidth = 0.6)

    p_main <- p_main + ggplot2::geom_text(
      data = ema_label_dt,
      ggplot2::aes(
        x = x_lab_ema,
        y = y,
        label = label,
        color = color,
        alpha = alpha
      ),
      hjust = -0.1,
      size = 3,
      show.legend = FALSE
    )
  }

  zone_alpha <- 0.15

  p_main <- p_main +
    ggplot2::geom_rect(
      data = line_dt[kind == "support"],
      ggplot2::aes(xmin = as.POSIXct(-Inf), xmax = as.POSIXct(Inf), ymin = zone_low, ymax = zone_high),
      inherit.aes = FALSE,
      fill = resolved$support,
      alpha = zone_alpha,
      color = NA
    ) +
    ggplot2::geom_hline(
      data = line_dt[kind == "support"],
      ggplot2::aes(yintercept = zone_center),
      color = resolved$support
    ) +
    ggplot2::geom_rect(
      data = line_dt[kind == "resistance"],
      ggplot2::aes(xmin = as.POSIXct(-Inf), xmax = as.POSIXct(Inf), ymin = zone_low, ymax = zone_high),
      inherit.aes = FALSE,
      fill = resolved$resistance,
      alpha = zone_alpha,
      color = NA
    ) +
    ggplot2::geom_hline(
      data = line_dt[kind == "resistance"],
      ggplot2::aes(yintercept = zone_center),
      color = resolved$resistance
    ) +
      ggplot2::geom_text(
        data = line_dt,
        ggplot2::aes(
          x = x_lab,
          y = zone_center + y_offset,
          label = formatC(round(zone_center, label_digits), format = "f", big.mark = ",", digits = label_digits),
          color = color
        ),
        hjust = -0.1,
        size = resolved$label_size,
        show.legend = FALSE
      )

  p_main <- p_main +
    ggplot2::scale_color_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(labels = scales::label_comma(big.mark = ",")) +
    ggplot2::labs(x = "", y = sprintf("%s (%s)", inst_id, bar)) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(
      xlim = c(x_min, x_max + (x_max - x_min) * 0.05),
      clip = "off"
    )
  p_main <- viz_theme_apply(p_main, style = resolved)
  p_main <- viz_annotate_last_value(p_main, DT, x = "datetime", y = "close", prefix = "Last: ", digits = label_digits, color = resolved$ink, style = resolved)

  p_main <- p_main +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  atr_label_dt <- data.table::data.table(
    x_lab_atr = max(DT$datetime),
    ema = c("atr_q_10", "atr_q_20", "atr_q_80", "atr_q_90"),
    y = as.numeric(DT[.N, .(atr_q_10_12_300, atr_q_20_12_300, atr_q_80_12_300, atr_q_90_12_300)]),
    label = c("10th Quantile", "20th Quantile", "80th Quantile", "90th Quantile")
  )

  atr_specs <- .viz_market_atr_specs(style = resolved)
  p_atr <- ggplot2::ggplot(DT[(.N + 1 - window_length):.N, ], ggplot2::aes(x = datetime, y = atr_logr_12)) +
    ggplot2::geom_line(ggplot2::aes(y = atr_q_10_12_300), linewidth = 0.4, linetype = "dotted", color = atr_specs$low) +
    ggplot2::geom_line(ggplot2::aes(y = atr_q_20_12_300), linewidth = 0.4, linetype = "dashed", color = atr_specs$mid) +
    ggplot2::geom_line(ggplot2::aes(y = atr_q_80_12_300), linewidth = 0.4, linetype = "dashed", color = atr_specs$mid) +
    ggplot2::geom_line(ggplot2::aes(y = atr_q_90_12_300), linewidth = 0.4, linetype = "dotted", color = atr_specs$high) +
    ggplot2::geom_line(linewidth = resolved$line_width, color = atr_specs$current) +
    ggplot2::labs(x = "", y = "ATR (log version)") +
    ggplot2::coord_cartesian(
      xlim = c(x_min, x_max + (x_max - x_min) * 0.05),
      clip = "off"
    )
  p_atr <- viz_theme_apply(p_atr, style = resolved)

  p_atr <- p_atr + ggplot2::geom_text(
    data = atr_label_dt,
    ggplot2::aes(
      x = x_lab_atr,
      y = y,
      label = label
    ),
    hjust = -0.1,
    size = resolved$label_size,
    show.legend = FALSE
  )

  compiler_caption <- if (isTRUE(show_compiler)) .investlabr_compiler_caption(NULL) else NULL
  p <- patchwork::wrap_plots(p_main, p_atr, ncol = 1, heights = c(4, 1)) +
    patchwork::plot_annotation(caption = compiler_caption) +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(5.5, 40, 5.5, 5.5)
    )

  invisible(p)
}
