#' @export
gen_plot_event_tsline_cum_ret <- function(res, threshold = 0.02) {
  wide_DT <- res$performance_by_horizons[[1]][, c('Horizon', '10%', '25%', '50%', '75%', '90%')]
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
gen_plot_comparing_events <- function(first_event_res, second_event_res, first_event_label, second_event_label, first_event_color = '#2ECC71', second_event_color = '#E74C3C') {
  event_res <- rbind(
    first_event_res$performance_by_horizons[[1]][, .(Horizon, Mean)][, pos:=first_event_label],
    second_event_res$performance_by_horizons[[1]][, .(Horizon, Mean)][, pos:=second_event_label]
    )
  event_res |> ggplot2::ggplot(ggplot2::aes(x = Horizon, y = Mean, group = pos, color = pos)) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::scale_color_manual(
      values = setNames(
        c(first_event_color, second_event_color),
        c(first_event_label, second_event_label)
      )
    ) +
    ggplot2::labs(x = '', y = '') +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

#' Generate candlestick plot with support and resistance lines
#'
#' @param DT A data.table containing OHLC data with columns \code{datetime}, \code{open}, \code{high}, \code{low}, and \code{close}.
#' @param support_pts Numeric vector of support price levels.
#' @param resistance_pts Numeric vector of resistance price levels.
#'
#' @return A ggplot2 object showing candlestick chart with support and resistance lines.
#' @export
gen_candle_plots_with_sr_lines <- function(DT, support_pts, resistance_pts) {

  DT[, candle_color := ifelse(open < close, "forestgreen", "firebrick")]
  body_half <- 0.45 * median(diff(as.numeric(DT$datetime)))
  wick_half <- body_half * 0.15
  
  DT |> ggplot2::ggplot(ggplot2::aes(x = datetime)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = datetime - wick_half,
        xmax = datetime + wick_half,
        ymin = low,
        ymax = high,
        fill = candle_color
      ),
      color = NA
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = datetime - body_half,
        xmax = datetime + body_half,
        ymin = pmin(open, close),
        ymax = pmax(open, close),
        fill = candle_color
      ),
      color = NA
    ) +
    ggplot2::geom_hline(yintercept = support_pts, color = 'steelblue') +
    ggplot2::geom_hline(yintercept = resistance_pts, color = 'darkorange') +
    ggplot2::labs(x = '', y = '') +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

.get_yield_of_date <- function(yield_dt, date_selected = NULL) {
  if (is.null(date_selected)) {
    return(yield_dt[.N, value])  
  } else {
    return(yield_dt[date <= date_selected, ][.N, value])
  }
}

#' Get yield data for given dates
#'
#' @param yield_dt_list A list of data.tables, each containing a yield series with columns \code{date} and \code{value}.
#' @param yield_dates A vector of dates for which yields should be retrieved.
#'
#' @return A data.table containing yield data across maturities and dates.
#' @export
get_yield_data_DT <- function(yield_dt_list, yield_dates) {
  maturity_months <- c(1, 3, 6, 12, 24, 36, 60, 84, 120, 240, 360)
  maturity_label <- c("1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")
  yield_data_list <- lapply(yield_dates, function(date_selected) {
    sapply(yield_dt_list, function(yield_dt) .get_yield_of_date(yield_dt, date_selected))
  })
  DT <- data.table::as.data.table(yield_data_list)
  DT[, maturity := maturity_months]
  DT[, log_maturity := log(maturity_months)]
  DT[, maturity_label := maturity_label]
  invisible(DT)
}

#' Generate U.S. Treasury yield curve plot
#'
#' @param DT A data.table returned from \code{get_yield_data_DT()}.
#' @param selected_windows Character vector specifying which yield curves to display (e.g., \code{"Now"}, \code{"One week ago"}).
#'
#' @return A ggplot2 object visualizing selected yield curves.
#' @export
gen_yield_curve_plot <- function(DT, selected_windows = c("Now", "One week ago", "One month ago")) {
  
  DT_long <- data.table::melt(
    DT,
    id.vars = c("maturity", "log_maturity", "maturity_label"),
    variable.name = "curve_date",
    value.name = "yield"
  )
  DT_long <- DT_long[curve_date %in% selected_windows,]
  
  okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
  
  DT_long |> ggplot2::ggplot(ggplot2::aes(x = log_maturity, y = yield, color = curve_date, group = curve_date)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_x_continuous(
      name   = "Maturity",
      breaks = DT$log_maturity,
      labels = DT$maturity_label
    ) +
    ggplot2::scale_color_manual(values = okabe_ito) +
    ggplot2::scale_y_continuous(name = "Yield (%)") +
    ggplot2::labs(title = "U.S. Treasury Yield Curve", color = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
	  plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.text = ggplot2::element_text(size = 9)  # adjust legend label size
    )
}

#' Generate yield curve comparison grid
#'
#' @param DT A data.table returned from \code{get_yield_data_DT()}.
#' @param selected_windows Character vector specifying yield curve snapshots for comparison.
#'
#' @return A faceted ggplot2 object comparing yield curves across multiple time windows.
#' @export
gen_yield_curve_plot_grid <- function(DT, selected_windows = c("Now", "One week ago", "One month ago", "Six months ago", "One year ago")) {
  if (is.null(selected_windows)) selected_windows <- c("Now", "One week ago", "One month ago", "Six months ago", "One year ago", "Two years ago", "Five years ago", "Ten years ago", "Fifteen years ago", "Twenty years ago")
  n_windows <- length(selected_windows)
  if (n_windows <= 4) {
    nrow_grids <- 1L
  } else if (n_windows <= 7) {
    nrow_grids <- 2L
  } else {
    nrow_grids <- 3L
  }
  DT_long <- data.table::melt.data.table(
      DT,
      id.vars = c("maturity", "log_maturity", "maturity_label", "Now"),
      variable.name = "before_window",
      value.name = "Before"
    )
  DT_long <- DT_long[before_window %in% selected_windows,]
  DT_long <- data.table::melt.data.table(
      DT_long,
      id.vars = c("maturity", "log_maturity", "maturity_label", "before_window"),
      variable.name = "panel",
      value.name = "yield"
    )
  
  DT_long |> ggplot2::ggplot(ggplot2::aes(x = log_maturity, y = yield, color = panel, group = panel)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::scale_x_continuous(
        name   = "Maturity",
        breaks = DT$log_maturity,
        labels = DT$maturity_label
      ) +
      ggplot2::scale_y_continuous(name = "Yield (%)") +
      ggplot2::facet_wrap(~ before_window, nrow = nrow_grids) +
      ggplot2::labs(title = "U.S. Treasury Yield Curve", color = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.text = ggplot2::element_text(size = 9)  # adjust legend label size
      )
}

#' @export
gen_grid_of_plots_with_labels <- function(
  plots,
  n_rows, n_cols,
  row_labs = NULL,              # character vector or NULL
  col_labs = NULL,              # character vector or NULL
  title    = NULL,              # character or NULL
  bottom   = NULL,              # character or NULL
  row_label_width_cm   = 2.2,   # width of left label column (if shown)
  col_header_height_cm = 1.2,   # height of top header row (if shown)
  row_label_gp = grid::gpar(fontface = "bold", cex = 0.95),
  col_label_gp = grid::gpar(fontface = "bold", cex = 1.0),
  title_gp     = grid::gpar(fontsize = 16, fontface = "bold"),
  bottom_gp    = grid::gpar(fontsize = 11)
) {
  stopifnot(length(plots) == n_rows * n_cols)
  
  # Treat NULL plots as blanks so they still occupy their cell
  blank_grob <- grid::nullGrob()
  plots <- lapply(plots, function(p) if (is.null(p)) blank_grob else p)

  show_row_labs <- !is.null(row_labs)
  show_col_labs <- !is.null(col_labs)
  if (show_row_labs) stopifnot(length(row_labs) == n_rows)
  if (show_col_labs) stopifnot(length(col_labs) == n_cols)

  n_layout_rows <- n_rows + as.integer(show_col_labs)
  n_layout_cols <- n_cols + as.integer(show_row_labs)

  # Build label grobs
  blank_grob <- grid::rectGrob(gp = grid::gpar(col = NA, fill = NA))
  col_header_grobs <- if (show_col_labs) {
    lapply(as.character(col_labs), function(lbl)
      grid::textGrob(lbl, gp = col_label_gp)
    )
  } else list()
  row_label_grobs <- if (show_row_labs) {
    lapply(row_labs, function(lbl)
      grid::textGrob(lbl, x = grid::unit(1, "npc"), hjust = 1, gp = row_label_gp)
    )
  } else list()

  # Assemble grob list in draw order and map into a layout matrix
  grobs <- list()
  lay   <- matrix(NA_integer_, nrow = n_layout_rows, ncol = n_layout_cols)
  idx <- 0L

  top_row    <- 1L
  left_col   <- 1L
  plot_row_0 <- if (show_col_labs) 2L else 1L
  plot_col_0 <- if (show_row_labs) 2L else 1L

  # Corner (only if both headers exist)
  if (show_row_labs && show_col_labs) {
    grobs[[length(grobs) + 1L]] <- blank_grob; idx <- idx + 1L
    lay[top_row, left_col] <- idx
  }

  # Column headers
  if (show_col_labs) {
    for (j in seq_len(n_cols)) {
      grobs[[length(grobs) + 1L]] <- col_header_grobs[[j]]; idx <- idx + 1L
      lay[top_row, plot_col_0 - 1L + j] <- idx
    }
  }

  # Row labels
  if (show_row_labs) {
    for (i in seq_len(n_rows)) {
      grobs[[length(grobs) + 1L]] <- row_label_grobs[[i]]; idx <- idx + 1L
      lay[plot_row_0 - 1L + i, left_col] <- idx
    }
  }

  # Plots (row-major)
  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      grobs[[length(grobs) + 1L]] <- plots[[ (i - 1L) * n_cols + j ]]
      idx <- idx + 1L
      lay[plot_row_0 - 1L + i, plot_col_0 - 1L + j] <- idx
    }
  }

  # Sizes
  widths <- if (show_row_labs) {
    grid::unit.c(grid::unit(row_label_width_cm, "cm"),
                 rep(grid::unit(1, "null"), n_cols))
  } else {
    rep(grid::unit(1, "null"), n_cols)
  }
  heights <- if (show_col_labs) {
    grid::unit.c(grid::unit(col_header_height_cm, "cm"),
                 rep(grid::unit(1, "null"), n_rows))
  } else {
    rep(grid::unit(1, "null"), n_rows)
  }

  # Title / bottom grobs (optional)
  top_grob    <- if (!is.null(title))  grid::textGrob(title,  gp = title_gp)  else NULL
  bottom_grob <- if (!is.null(bottom)) grid::textGrob(bottom, gp = bottom_gp) else NULL

  gridExtra::grid.arrange(
    grobs = grobs,
    layout_matrix = lay,
    widths  = widths,
    heights = heights,
    top     = top_grob,
    bottom  = bottom_grob
  )
}