# Lightweight gallery glue only. Reusable forecast preparation, simulation,
# scoring, and plotting logic lives in investlabr package functions.
library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

mf_style <- "policy_memo"
mf_context <- "report"

mf_theme <- function(p, legend_position = "bottom", show_compiler = FALSE) {
  investlabr::viz_theme_apply(
    p,
    style = mf_style,
    context = mf_context,
    legend_position = legend_position,
    show_compiler = show_compiler
  )
}

mf_load_fred <- function(series_id, label = series_id, start_date = as.Date("1990-01-01")) {
  dt <- tryCatch(
    data.table::as.data.table(investdatar::get_local_FRED_data(series_id)),
    error = function(e) NULL
  )
  if (is.null(dt) || nrow(dt) == 0L || !"value" %in% names(dt)) {
    stop(
      "Missing local FRED series: ", series_id,
      ". Sync it with investdatar::sync_local_fred_data(\"", series_id, "\").",
      call. = FALSE
    )
  }
  dt[, date := as.Date(date)]
  dt[, value := as.numeric(value)]
  dt <- dt[date >= start_date & is.finite(value), .(date, value, series = label)]
  data.table::setorder(dt, date)
  if (nrow(dt) == 0L) stop("No usable rows for FRED series: ", series_id, call. = FALSE)
  dt
}

mf_try_fred <- function(series_id, label = series_id, start_date = as.Date("1990-01-01")) {
  tryCatch(
    mf_load_fred(series_id, label, start_date),
    error = function(e) {
      message("Skipping unavailable FRED series ", series_id, ": ", conditionMessage(e))
      NULL
    }
  )
}

mf_load_yahoo <- function(symbol, label = symbol, start_date = as.Date("1990-01-01")) {
  dt <- tryCatch(
    data.table::as.data.table(investdatar::get_local_quantmod_OHLC(symbol, src = "yahoo")),
    error = function(e) NULL
  )
  if (is.null(dt) || nrow(dt) == 0L) {
    stop(
      "Missing local Yahoo OHLC data: ", symbol,
      ". Sync it with investdatar::sync_local_quantmod_OHLC(\"", symbol,
      "\", src = \"yahoo\").",
      call. = FALSE
    )
  }
  price_col <- if ("adj_close" %in% names(dt) && any(is.finite(dt$adj_close))) "adj_close" else "close"
  out <- dt[, .(date = as.Date(date), price = as.numeric(get(price_col)), symbol = label)]
  out <- out[date >= start_date & is.finite(price) & price > 0]
  data.table::setorder(out, date)
  if (nrow(out) == 0L) stop("No usable Yahoo rows for symbol: ", symbol, call. = FALSE)
  out
}

mf_plot_fan <- function(fan_dt, title, subtitle, y_label, accent = NULL) {
  investlabr::viz_forward_fan(
    fan_dt,
    title = title,
    subtitle = subtitle,
    y_label = y_label,
    accent = accent,
    style = mf_style,
    context = mf_context,
    show_compiler = FALSE
  )
}

mf_plot_lines <- function(dt, x, y, color = "series", title, subtitle, y_label, colors = NULL) {
  investlabr::viz_series_lines(
    dt,
    x = x,
    y = y,
    color = color,
    title = title,
    subtitle = subtitle,
    y_label = y_label,
    colors = colors,
    style = mf_style,
    context = mf_context,
    show_compiler = FALSE
  )
}
