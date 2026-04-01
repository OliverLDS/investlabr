#' @inheritParams viz_style_get
#' @export
gen_grid_of_plots_with_labels <- function(
  plots,
  n_rows, n_cols,
  row_labs = NULL,
  col_labs = NULL,
  title = NULL,
  bottom = NULL,
  row_label_width_cm = 2.2,
  col_header_height_cm = 1.2,
  row_label_gp = NULL,
  col_label_gp = NULL,
  title_gp = NULL,
  bottom_gp = NULL,
  style = NULL,
  context = NULL
) {
  resolved <- .viz_resolve_style(style = style, context = context)
  if (is.null(row_label_gp)) row_label_gp <- grid::gpar(fontface = "bold", cex = resolved$base_size / 11, col = resolved$ink)
  if (is.null(col_label_gp)) col_label_gp <- grid::gpar(fontface = "bold", cex = resolved$base_size / 10, col = resolved$ink)
  if (is.null(title_gp)) title_gp <- grid::gpar(fontsize = resolved$title_size, fontface = "bold", col = resolved$ink)
  if (is.null(bottom_gp)) bottom_gp <- grid::gpar(fontsize = resolved$caption_size + 2, col = resolved$muted)
  stopifnot(length(plots) == n_rows * n_cols)

  blank_grob <- grid::nullGrob()
  plots <- lapply(plots, function(p) if (is.null(p)) blank_grob else p)

  show_row_labs <- !is.null(row_labs)
  show_col_labs <- !is.null(col_labs)
  if (show_row_labs) stopifnot(length(row_labs) == n_rows)
  if (show_col_labs) stopifnot(length(col_labs) == n_cols)

  n_layout_rows <- n_rows + as.integer(show_col_labs)
  n_layout_cols <- n_cols + as.integer(show_row_labs)

  blank_grob <- grid::rectGrob(gp = grid::gpar(col = NA, fill = NA))
  col_header_grobs <- if (show_col_labs) {
    lapply(as.character(col_labs), function(lbl) grid::textGrob(lbl, gp = col_label_gp))
  } else list()
  row_label_grobs <- if (show_row_labs) {
    lapply(row_labs, function(lbl) grid::textGrob(lbl, x = grid::unit(1, "npc"), hjust = 1, gp = row_label_gp))
  } else list()

  grobs <- list()
  lay <- matrix(NA_integer_, nrow = n_layout_rows, ncol = n_layout_cols)
  idx <- 0L

  top_row <- 1L
  left_col <- 1L
  plot_row_0 <- if (show_col_labs) 2L else 1L
  plot_col_0 <- if (show_row_labs) 2L else 1L

  if (show_row_labs && show_col_labs) {
    grobs[[length(grobs) + 1L]] <- blank_grob
    idx <- idx + 1L
    lay[top_row, left_col] <- idx
  }

  if (show_col_labs) {
    for (j in seq_len(n_cols)) {
      grobs[[length(grobs) + 1L]] <- col_header_grobs[[j]]
      idx <- idx + 1L
      lay[top_row, plot_col_0 - 1L + j] <- idx
    }
  }

  if (show_row_labs) {
    for (i in seq_len(n_rows)) {
      grobs[[length(grobs) + 1L]] <- row_label_grobs[[i]]
      idx <- idx + 1L
      lay[plot_row_0 - 1L + i, left_col] <- idx
    }
  }

  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      grobs[[length(grobs) + 1L]] <- plots[[(i - 1L) * n_cols + j]]
      idx <- idx + 1L
      lay[plot_row_0 - 1L + i, plot_col_0 - 1L + j] <- idx
    }
  }

  widths <- if (show_row_labs) {
    grid::unit.c(grid::unit(row_label_width_cm, "cm"), rep(grid::unit(1, "null"), n_cols))
  } else {
    rep(grid::unit(1, "null"), n_cols)
  }
  heights <- if (show_col_labs) {
    grid::unit.c(grid::unit(col_header_height_cm, "cm"), rep(grid::unit(1, "null"), n_rows))
  } else {
    rep(grid::unit(1, "null"), n_rows)
  }

  top_grob <- if (!is.null(title)) grid::textGrob(title, gp = title_gp) else NULL
  bottom_grob <- if (!is.null(bottom)) grid::textGrob(bottom, gp = bottom_gp) else NULL

  gridExtra::grid.arrange(
    grobs = grobs,
    layout_matrix = lay,
    widths = widths,
    heights = heights,
    top = top_grob,
    bottom = bottom_grob
  )
}

#' @inheritParams viz_style_get
#' @export
gen_facet_plot_from_multicol_ts <- function(DT, id_vars, measure_vars, measure_labels, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  DT_long <- data.table::melt(
    DT,
    id.vars = id_vars,
    measure.vars = measure_vars,
    variable.name = "metric",
    value.name = "value"
  )

  DT_long[, metric := factor(
    metric,
    levels = measure_vars,
    labels = measure_labels
  )]

  p <- ggplot2::ggplot(DT_long, ggplot2::aes_string(x = id_vars, y = "value")) +
    ggplot2::geom_line(color = resolved$accent, linewidth = resolved$line_width) +
    ggplot2::geom_point(color = resolved$accent2, size = resolved$point_size) +
    ggplot2::facet_grid(metric ~ ., scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL)
  viz_theme_apply(p, style = resolved)
}
