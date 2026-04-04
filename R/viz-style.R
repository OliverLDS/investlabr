.viz_style_catalog <- function() {
  list(
    research_note = list(
      name = "research_note",
      ink = "#1F2933",
      paper = "#FAF8F2",
      grid = "#D9D4C7",
      muted = "#6B7280",
      accent = "#0F4C5C",
      accent2 = "#C97A2B",
      up = "#2D6A4F",
      down = "#9B2226",
      support = "#2B6CB0",
      resistance = "#C05621",
      discrete = c("#0F4C5C", "#C97A2B", "#5A6C57", "#B56576", "#3D5A80", "#6D597A"),
      sequential = c("#DCEAF2", "#9BC1D9", "#5F8FB1", "#2F5F85", "#173753")
    ),
    macro_classic = list(
      name = "macro_classic",
      ink = "#27231F",
      paper = "#F6F1E7",
      grid = "#D6CCBC",
      muted = "#736B5E",
      accent = "#AA3A2A",
      accent2 = "#214E63",
      up = "#3B7A57",
      down = "#A73A2D",
      support = "#356D9A",
      resistance = "#B45F06",
      discrete = c("#AA3A2A", "#214E63", "#6B8E23", "#7B4F9E", "#C07C2B", "#497D74"),
      sequential = c("#F6E3D4", "#E6BFA0", "#D28C69", "#B85C38", "#8F3320")
    ),
    terminal_risk = list(
      name = "terminal_risk",
      ink = "#E6EDF3",
      paper = "#09131A",
      grid = "#21303B",
      muted = "#94A3B8",
      accent = "#2EC4B6",
      accent2 = "#FFB703",
      up = "#5BE37D",
      down = "#FF6B6B",
      support = "#4EA8DE",
      resistance = "#FF9F1C",
      discrete = c("#2EC4B6", "#FFB703", "#8ECAE6", "#FB8500", "#90BE6D", "#B388EB"),
      sequential = c("#173042", "#1F5673", "#2A7CA4", "#5BA9D0", "#A8DADC")
    ),
    cross_asset_color = list(
      name = "cross_asset_color",
      ink = "#18212B",
      paper = "#FCFCFB",
      grid = "#DCE2E8",
      muted = "#6B7280",
      accent = "#1D3557",
      accent2 = "#E76F51",
      up = "#2A9D8F",
      down = "#D62828",
      support = "#457B9D",
      resistance = "#E76F51",
      discrete = c("#1D3557", "#E76F51", "#2A9D8F", "#6A4C93", "#F4A261", "#4D908E"),
      sequential = c("#E8F1F8", "#BAD7EA", "#7AA5C4", "#457B9D", "#1D3557")
    ),
    minimal_print = list(
      name = "minimal_print",
      ink = "#222222",
      paper = "#FFFFFF",
      grid = "#D9D9D9",
      muted = "#7A7A7A",
      accent = "#404040",
      accent2 = "#6B6B6B",
      up = "#404040",
      down = "#808080",
      support = "#5A5A5A",
      resistance = "#2F2F2F",
      discrete = c("#202020", "#4D4D4D", "#777777", "#9D9D9D", "#BBBBBB", "#606060"),
      sequential = c("#F2F2F2", "#D9D9D9", "#BFBFBF", "#808080", "#404040")
    ),
    presentation_bold = list(
      name = "presentation_bold",
      ink = "#1A1A1A",
      paper = "#FFF9F0",
      grid = "#E6DDCF",
      muted = "#6B705C",
      accent = "#C1121F",
      accent2 = "#003049",
      up = "#2A9D8F",
      down = "#D62828",
      support = "#1D3557",
      resistance = "#E76F51",
      discrete = c("#C1121F", "#003049", "#2A9D8F", "#F77F00", "#6A4C93", "#588157"),
      sequential = c("#FDE2E4", "#F9BEC7", "#F497A9", "#E85D75", "#C1121F")
    ),
    briefing_serif = list(
      name = "briefing_serif",
      ink = "#2B2620",
      paper = "#FBF8F1",
      grid = "#DDD3C3",
      muted = "#756B5B",
      accent = "#7D3C2E",
      accent2 = "#355C7D",
      up = "#4E7A60",
      down = "#A64B3C",
      support = "#4F6D8A",
      resistance = "#9A5B2F",
      discrete = c("#7D3C2E", "#355C7D", "#6B7A3F", "#A46C5A", "#6E5A7B", "#8C8C5A"),
      sequential = c("#F5EBDD", "#E6D1B3", "#CFA27D", "#A86C45", "#7D3C2E")
    ),
    institutional_blue = list(
      name = "institutional_blue",
      ink = "#1E2A36",
      paper = "#F8FAFC",
      grid = "#D5DEE8",
      muted = "#627286",
      accent = "#163A5F",
      accent2 = "#6E8CA6",
      up = "#2F6B5F",
      down = "#B24A4A",
      support = "#3D6D99",
      resistance = "#8C5A3C",
      discrete = c("#163A5F", "#4F6D8C", "#6E8CA6", "#8FA8BC", "#3B5B7A", "#8097AA"),
      sequential = c("#E8EFF5", "#C6D4E1", "#99AFC4", "#5E7D9A", "#163A5F")
    ),
    policy_memo = list(
      name = "policy_memo",
      ink = "#2C2F2B",
      paper = "#F7F6F2",
      grid = "#D9DED4",
      muted = "#687166",
      accent = "#567A6E",
      accent2 = "#B07D4F",
      up = "#5D8A72",
      down = "#B45D4F",
      support = "#78909C",
      resistance = "#B07D4F",
      discrete = c("#567A6E", "#B07D4F", "#7C8C62", "#8E6C88", "#5E81AC", "#A3A380"),
      sequential = c("#EBF1EE", "#CDDCD5", "#A8BEB3", "#789B8D", "#567A6E")
    ),
    desk_monitor = list(
      name = "desk_monitor",
      ink = "#E8EEF2",
      paper = "#0D141A",
      grid = "#253540",
      muted = "#93A4B3",
      accent = "#4CC9F0",
      accent2 = "#F4A261",
      up = "#6DD3A0",
      down = "#F28482",
      support = "#4895EF",
      resistance = "#FFB703",
      discrete = c("#4CC9F0", "#F4A261", "#90BE6D", "#B388EB", "#E76F51", "#A8DADC"),
      sequential = c("#18242D", "#244150", "#2E6175", "#4A8FB0", "#8BC7E0")
    ),
    client_slide = list(
      name = "client_slide",
      ink = "#1C1C1C",
      paper = "#FFFDF8",
      grid = "#E7E0D3",
      muted = "#6E6A61",
      accent = "#C44536",
      accent2 = "#1F4E79",
      up = "#2A9D8F",
      down = "#D1495B",
      support = "#1F4E79",
      resistance = "#F4A261",
      discrete = c("#C44536", "#1F4E79", "#2A9D8F", "#F4A261", "#6A4C93", "#7A9E7E"),
      sequential = c("#FDE9E4", "#F8C9BC", "#EE9B88", "#E06B56", "#C44536")
    ),
    newswire_print = list(
      name = "newswire_print",
      ink = "#202020",
      paper = "#FFFFFF",
      grid = "#CFCFCF",
      muted = "#666666",
      accent = "#2B2B2B",
      accent2 = "#5C5C5C",
      up = "#2B2B2B",
      down = "#7A7A7A",
      support = "#4D4D4D",
      resistance = "#1F1F1F",
      discrete = c("#202020", "#4A4A4A", "#6B6B6B", "#8A8A8A", "#A6A6A6", "#5A5A5A"),
      sequential = c("#F4F4F4", "#D9D9D9", "#BEBEBE", "#7F7F7F", "#333333")
    )
  )
}

.viz_context_catalog <- function() {
  list(
    report = list(
      base_size = 11,
      title_size = 14,
      subtitle_size = 10,
      caption_size = 8,
      strip_size = 9,
      legend_text_size = 8.5,
      axis_text_size = 9,
      axis_title_size = 10,
      line_width = 0.9,
      point_size = 1.8,
      label_size = 3.1,
      plot_margin = ggplot2::margin(7, 18, 7, 7),
      legend_position = "bottom"
    ),
    slide = list(
      base_size = 15,
      title_size = 22,
      subtitle_size = 14,
      caption_size = 10,
      strip_size = 13,
      legend_text_size = 12,
      axis_text_size = 12,
      axis_title_size = 13,
      line_width = 1.4,
      point_size = 2.8,
      label_size = 4.2,
      plot_margin = ggplot2::margin(12, 28, 12, 12),
      legend_position = "bottom"
    ),
    dashboard = list(
      base_size = 10,
      title_size = 12,
      subtitle_size = 9,
      caption_size = 7,
      strip_size = 8,
      legend_text_size = 8,
      axis_text_size = 8,
      axis_title_size = 9,
      line_width = 0.8,
      point_size = 1.5,
      label_size = 2.8,
      plot_margin = ggplot2::margin(5, 12, 5, 5),
      legend_position = "right"
    )
  )
}

.viz_merge_style <- function(style, context) {
  utils::modifyList(style, context)
}

.viz_market_ema_specs <- function(style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  cols <- rep_len(resolved$discrete, 4)
  list(
    colors = stats::setNames(cols, c("ema_20", "ema_50", "ema_100", "ema_200")),
    alpha = stats::setNames(c(0.95, 0.85, 0.75, 0.65), c("ema_20", "ema_50", "ema_100", "ema_200"))
  )
}

.viz_market_atr_specs <- function(style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  list(
    low = resolved$muted,
    mid = resolved$accent,
    high = resolved$accent2,
    current = resolved$ink
  )
}

.viz_resolve_style <- function(style = NULL, context = NULL) {
  if (is.list(style)) return(style)

  style_name <- if (is.null(style)) getOption("investlabr.viz.style", "research_note") else style
  context_name <- if (is.null(context)) getOption("investlabr.viz.context", "report") else context

  style_catalog <- .viz_style_catalog()
  context_catalog <- .viz_context_catalog()

  if (!style_name %in% names(style_catalog)) {
    stop(sprintf("Unknown style '%s'. Available styles: %s", style_name, paste(names(style_catalog), collapse = ", ")), call. = FALSE)
  }
  if (!context_name %in% names(context_catalog)) {
    stop(sprintf("Unknown context '%s'. Available contexts: %s", context_name, paste(names(context_catalog), collapse = ", ")), call. = FALSE)
  }

  resolved <- .viz_merge_style(style_catalog[[style_name]], context_catalog[[context_name]])
  resolved$context <- context_name
  resolved
}

#' Set default visualization style and context
#'
#' @param style Named style family.
#' @param context Output context such as `"report"`, `"slide"`, or `"dashboard"`.
#'
#' @return Invisibly returns the resolved style object.
#' @export
viz_style_set <- function(style = "research_note", context = "report") {
  options(
    investlabr.viz.style = style,
    investlabr.viz.context = context
  )
  invisible(.viz_resolve_style(style = style, context = context))
}

#' Get resolved visualization style
#'
#' @param style Named style family or `NULL` to use the package default.
#' @param context Output context or `NULL` to use the package default.
#'
#' @return Named list describing colors, sizing, and theme settings.
#' @export
viz_style_get <- function(style = NULL, context = NULL) {
  .viz_resolve_style(style = style, context = context)
}

#' Get visualization palette values
#'
#' @param style Named style family or `NULL` to use the package default.
#' @param context Output context or `NULL` to use the package default.
#' @param palette One of `"discrete"` or `"sequential"`.
#'
#' @return Character vector of colors.
#' @export
viz_palette_get <- function(style = NULL, context = NULL, palette = c("discrete", "sequential")) {
  palette <- match.arg(palette)
  resolved <- .viz_resolve_style(style = style, context = context)
  resolved[[palette]]
}

#' Apply a standardized investlabr plot theme
#'
#' @param plot A `ggplot` object.
#' @param style Named style family or `NULL` to use the package default.
#' @param context Output context or `NULL` to use the package default.
#' @param legend_position Optional legend position override.
#' @param base_family Optional font family override.
#' @param show_compiler Whether to append the configured plot compiler name to the caption.
#'
#' @return A styled `ggplot` object.
#' @export
viz_theme_apply <- function(plot, style = NULL, context = NULL, legend_position = NULL, base_family = NULL, show_compiler = TRUE) {
  resolved <- .viz_resolve_style(style = style, context = context)
  if (!is.null(base_family)) resolved$base_family <- base_family
  if (!is.null(legend_position)) resolved$legend_position <- legend_position
  if (isTRUE(show_compiler)) {
    caption <- .investlabr_compiler_caption(plot$labels$caption)
    if (!identical(caption, plot$labels$caption)) {
      plot <- plot + ggplot2::labs(caption = caption)
    }
  }

  theme_base <- ggplot2::theme_minimal(
    base_size = resolved$base_size,
    base_family = if (!is.null(resolved$base_family)) resolved$base_family else ""
  )

  plot +
    theme_base +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = resolved$paper, color = NA),
      panel.background = ggplot2::element_rect(fill = resolved$paper, color = NA),
      panel.grid.major = ggplot2::element_line(color = resolved$grid, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(color = resolved$ink, face = "bold", size = resolved$title_size),
      plot.subtitle = ggplot2::element_text(color = resolved$muted, size = resolved$subtitle_size),
      plot.caption = ggplot2::element_text(color = resolved$muted, size = resolved$caption_size, hjust = 0),
      axis.text = ggplot2::element_text(color = resolved$ink, size = resolved$axis_text_size),
      axis.title = ggplot2::element_text(color = resolved$ink, size = resolved$axis_title_size),
      strip.background = ggplot2::element_rect(fill = scales::alpha(resolved$grid, 0.3), color = NA),
      strip.text = ggplot2::element_text(color = resolved$ink, size = resolved$strip_size, face = "bold"),
      legend.position = resolved$legend_position,
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = resolved$ink, size = resolved$legend_text_size),
      plot.margin = resolved$plot_margin
    )
}

#' Add a direct label for the last observation in a series
#'
#' @param plot A `ggplot` object.
#' @param data A data.frame or data.table containing the plotted series.
#' @param x Name of the x column.
#' @param y Name of the y column.
#' @param label Optional name of a label column. If `NULL`, the formatted `y` value is used.
#' @param prefix Optional prefix prepended to the label text.
#' @param digits Number of digits used when formatting numeric labels.
#' @param color Optional label color.
#' @param style Named style family or `NULL` to use the package default.
#' @param context Output context or `NULL` to use the package default.
#'
#' @return A `ggplot` object with a last-value annotation.
#' @export
viz_annotate_last_value <- function(plot, data, x, y, label = NULL, prefix = "", digits = 2, color = NULL, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  data <- data.table::as.data.table(data)
  last_row <- data[stats::complete.cases(data[, c(x, y), with = FALSE]), ][.N]
  if (nrow(last_row) == 0L) return(plot)

  label_value <- if (is.null(label)) {
    paste0(prefix, formatC(last_row[[y]], format = "f", digits = digits, big.mark = ","))
  } else {
    paste0(prefix, last_row[[label]])
  }
  last_row[, label_value := label_value]

  plot +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes_string(x = x, y = y, label = "label_value"),
      inherit.aes = FALSE,
      color = if (is.null(color)) resolved$ink else color,
      hjust = -0.1,
      size = resolved$label_size
    )
}

#' Add event-line annotations to a plot
#'
#' @param plot A `ggplot` object.
#' @param event_dt Data with at least an x-position column.
#' @param x_col Name of the x-position column.
#' @param label_col Optional label column.
#' @param color Optional line and label color.
#' @param linetype Line type for event markers.
#' @param alpha Alpha applied to event lines.
#' @param style Named style family or `NULL` to use the package default.
#' @param context Output context or `NULL` to use the package default.
#'
#' @return A `ggplot` object with event annotations.
#' @export
viz_annotate_event_lines <- function(plot, event_dt, x_col = "datetime", label_col = NULL, color = NULL, linetype = "dashed", alpha = 0.45, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  event_color <- if (is.null(color)) resolved$muted else color
  event_dt <- data.table::as.data.table(event_dt)
  p <- plot +
    ggplot2::geom_vline(
      data = event_dt,
      ggplot2::aes_string(xintercept = x_col),
      inherit.aes = FALSE,
      color = event_color,
      linetype = linetype,
      alpha = alpha
    )

  if (!is.null(label_col)) {
    p <- p +
      ggplot2::geom_text(
        data = event_dt,
        ggplot2::aes_string(x = x_col, label = label_col),
        inherit.aes = FALSE,
        y = Inf,
        color = event_color,
        vjust = 1.2,
        angle = 90,
        size = resolved$label_size
      )
  }

  p
}

#' Add regime-band annotations to a plot
#'
#' @param plot A `ggplot` object.
#' @param regime_dt Data with start and end columns.
#' @param xmin_col Name of the regime start column.
#' @param xmax_col Name of the regime end column.
#' @param fill Optional fill color.
#' @param alpha Optional fill alpha.
#' @param style Named style family or `NULL` to use the package default.
#' @param context Output context or `NULL` to use the package default.
#'
#' @return A `ggplot` object with shaded regime bands.
#' @export
viz_annotate_regime_bands <- function(plot, regime_dt, xmin_col = "start", xmax_col = "end", fill = NULL, alpha = 0.12, style = NULL, context = NULL) {
  resolved <- .viz_resolve_style(style = style, context = context)
  regime_dt <- data.table::as.data.table(regime_dt)
  plot +
    ggplot2::geom_rect(
      data = regime_dt,
      ggplot2::aes_string(xmin = xmin_col, xmax = xmax_col),
      inherit.aes = FALSE,
      ymin = -Inf,
      ymax = Inf,
      fill = if (is.null(fill)) resolved$accent else fill,
      alpha = alpha,
      color = NA
    )
}

#' Append a standardized source caption to a plot
#'
#' @param plot A `ggplot` object.
#' @param source_text Source text.
#' @param note Optional note appended after the source.
#'
#' @return A `ggplot` object with an updated caption.
#' @export
viz_annotate_source_caption <- function(plot, source_text, note = NULL) {
  caption <- if (is.null(note) || !nzchar(note)) {
    paste0("Source: ", source_text)
  } else {
    paste0("Source: ", source_text, " | ", note)
  }
  plot + ggplot2::labs(caption = caption)
}
