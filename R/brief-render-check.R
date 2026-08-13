.brief_svg_check_time_extent <- function(
  svg_path,
  latest_dates,
  n_rows,
  n_cols,
  tolerance_px = 3
) {
  stopifnot(
    length(svg_path) == 1L,
    file.exists(svg_path),
    length(latest_dates) == n_rows * n_cols
  )

  svg <- readLines(svg_path, warn = FALSE)
  view_box_line <- svg[grepl("viewBox=", svg, fixed = TRUE)][1L]
  view_box <- sub(".*viewBox='([^']+)'.*", "\\1", view_box_line)
  dimensions <- as.numeric(strsplit(view_box, "[[:space:]]+")[[1L]])
  if (length(dimensions) != 4L || any(!is.finite(dimensions))) {
    stop("Could not read the SVG viewBox: ", svg_path, call. = FALSE)
  }
  svg_width <- dimensions[3L]
  svg_height <- dimensions[4L]

  label_lines <- svg[grepl(">[0-9]{4}-[0-9]{2}</text>", svg)]
  labels <- data.frame(
    x = as.numeric(sub(".*<text x='([^']+)'.*", "\\1", label_lines)),
    y = as.numeric(sub(".* y='([^']+)'.*", "\\1", label_lines)),
    label = sub(".*>([0-9]{4}-[0-9]{2})</text>.*", "\\1", label_lines),
    stringsAsFactors = FALSE
  )

  line_rows <- lapply(svg[grepl("<polyline points=", svg, fixed = TRUE)], function(line) {
    width_text <- sub(".*stroke-width: ([0-9.]+);.*", "\\1", line)
    width <- suppressWarnings(as.numeric(width_text))
    if (!is.finite(width) || width < 1.8 || width > 2.1) return(NULL)
    points_text <- sub(".*<polyline points='([^']+)'.*", "\\1", line)
    points <- strsplit(trimws(points_text), "[[:space:]]+")[[1L]]
    last_point <- strsplit(points[length(points)], ",", fixed = TRUE)[[1L]]
    if (length(last_point) != 2L) return(NULL)
    data.frame(
      x = as.numeric(last_point[1L]),
      y = as.numeric(last_point[2L]),
      stringsAsFactors = FALSE
    )
  })
  endpoints <- do.call(rbind, Filter(Negate(is.null), line_rows))
  if (nrow(labels) == 0L || is.null(endpoints) || nrow(endpoints) == 0L) {
    stop("Could not identify dated axes and data-line endpoints in SVG: ", svg_path, call. = FALSE)
  }

  panel_results <- vector("list", length(latest_dates))
  latest_dates <- as.Date(latest_dates)
  for (i in seq_along(latest_dates)) {
    row <- (i - 1L) %/% n_cols + 1L
    col <- (i - 1L) %% n_cols + 1L
    x_min <- (col - 1L) * svg_width / n_cols
    x_max <- col * svg_width / n_cols
    y_min <- (row - 1L) * svg_height / n_rows
    y_max <- row * svg_height / n_rows

    panel_labels <- labels[
      labels$x >= x_min & labels$x < x_max & labels$y >= y_min & labels$y < y_max,
      ,
      drop = FALSE
    ]
    panel_endpoints <- endpoints[
      endpoints$x >= x_min & endpoints$x < x_max & endpoints$y >= y_min & endpoints$y < y_max,
      ,
      drop = FALSE
    ]
    month_start <- as.Date(paste0(format(latest_dates[i], "%Y-%m"), "-01"))
    previous_month <- format(month_start - 1L, "%Y-%m")
    current_month <- format(month_start, "%Y-%m")
    previous_x <- panel_labels$x[panel_labels$label == previous_month]
    current_x <- panel_labels$x[panel_labels$label == current_month]
    if (length(previous_x) != 1L || length(current_x) != 1L || nrow(panel_endpoints) == 0L) {
      stop(
        "Could not resolve dated extent for SVG panel ", i,
        " (latest observation ", latest_dates[i], ").",
        call. = FALSE
      )
    }

    prior_month_start <- as.Date(paste0(previous_month, "-01"))
    pixels_per_day <- (current_x - previous_x) /
      as.numeric(month_start - prior_month_start)
    expected_x <- current_x +
      as.numeric(latest_dates[i] - month_start) * pixels_per_day
    visible_x <- max(panel_endpoints$x)
    if (visible_x + tolerance_px < expected_x) {
      stop(
        "Rendered data paths end before the latest observation in SVG panel ", i,
        ": visible x=", round(visible_x, 2),
        ", expected x=", round(expected_x, 2),
        " for ", latest_dates[i], ".",
        call. = FALSE
      )
    }
    panel_results[[i]] <- data.frame(
      panel = i,
      latest_date = format(latest_dates[i], "%Y-%m-%d"),
      visible_x = visible_x,
      expected_x = expected_x,
      stringsAsFactors = FALSE
    )
  }

  list(
    visible_data_as_of = format(min(latest_dates), "%Y-%m-%d"),
    panels = do.call(rbind, panel_results)
  )
}
