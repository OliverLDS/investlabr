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
  dt <- tryCatch(data.table::as.data.table(investdatar::get_local_FRED_data(series_id)), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0L || !"value" %in% names(dt)) {
    stop("Missing local FRED series: ", series_id, ". Sync it with investdatar::sync_local_fred_data(\"", series_id, "\").", call. = FALSE)
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
  dt <- tryCatch(data.table::as.data.table(investdatar::get_local_quantmod_OHLC(symbol, src = "yahoo")), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0L) {
    stop("Missing local Yahoo OHLC data: ", symbol, ". Sync it with investdatar::sync_local_quantmod_OHLC(\"", symbol, "\", src = \"yahoo\").", call. = FALSE)
  }
  price_col <- if ("adj_close" %in% names(dt) && any(is.finite(dt$adj_close))) "adj_close" else "close"
  out <- dt[, .(date = as.Date(date), price = as.numeric(get(price_col)), symbol = label)]
  out <- out[date >= start_date & is.finite(price) & price > 0]
  data.table::setorder(out, date)
  if (nrow(out) == 0L) stop("No usable Yahoo rows for symbol: ", symbol, call. = FALSE)
  out
}

mf_to_wide <- function(series_list) {
  long <- data.table::rbindlist(series_list, fill = TRUE)
  out <- data.table::dcast(long, date ~ series, value.var = "value")
  data.table::setorder(out, date)
  value_cols <- setdiff(names(out), "date")
  out[, (value_cols) := lapply(.SD, data.table::nafill, type = "locf"), .SDcols = value_cols]
  out
}

mf_locf <- function(x) {
  if (length(x) == 0L) return(x)
  out <- x
  last <- NA
  for (i in seq_along(out)) {
    if (!is.na(out[i])) {
      last <- out[i]
    } else if (!is.na(last)) {
      out[i] <- last
    }
  }
  out
}

mf_zscore <- function(x) {
  sdv <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / sdv
}

mf_bound_score <- function(z, center = 0, scale = 1.2) {
  100 / (1 + exp(-(z - center) / scale))
}

mf_recent_changes <- function(x, lookback = 252L) {
  x <- x[is.finite(x)]
  dx <- diff(x)
  tail(dx[is.finite(dx)], lookback)
}

mf_fan_dt <- function(start_value, changes, horizons = c(1L, 5L, 10L, 20L, 60L), n_paths = 600L, seed = 1L) {
  if (length(changes) < 20L) stop("Not enough historical changes to build a fan.", call. = FALSE)
  set.seed(seed)
  paths <- vector("list", n_paths)
  max_h <- max(horizons)
  for (i in seq_len(n_paths)) {
    sampled <- sample(changes, size = max_h, replace = TRUE)
    paths[[i]] <- data.table(path = i, horizon = seq_len(max_h), value = start_value + cumsum(sampled))
  }
  path_dt <- data.table::rbindlist(paths)
  fan <- path_dt[horizon %in% horizons, .(
    p10 = stats::quantile(value, 0.10, na.rm = TRUE),
    p25 = stats::quantile(value, 0.25, na.rm = TRUE),
    p50 = stats::quantile(value, 0.50, na.rm = TRUE),
    p75 = stats::quantile(value, 0.75, na.rm = TRUE),
    p90 = stats::quantile(value, 0.90, na.rm = TRUE)
  ), by = horizon]
  start <- data.table(horizon = 0L, p10 = start_value, p25 = start_value, p50 = start_value, p75 = start_value, p90 = start_value)
  data.table::rbindlist(list(start, fan), use.names = TRUE)
}

mf_plot_fan <- function(fan_dt, title, subtitle, y_label, accent = "#1D4E89") {
  p <- ggplot(fan_dt, aes(horizon)) +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = accent, alpha = 0.16) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = accent, alpha = 0.28) +
    geom_line(aes(y = p50), color = accent, linewidth = 0.9) +
    geom_point(aes(y = p50), color = accent, size = 1.8) +
    labs(title = title, subtitle = subtitle, x = "Forward business days", y = y_label)
  mf_theme(p, legend_position = "none")
}

mf_scenario_path <- function(start_value, scenario_changes, horizon_months = 6L, start_date = Sys.Date()) {
  dates <- seq(as.Date(start_date), by = "month", length.out = horizon_months + 1L)
  data.table::rbindlist(lapply(names(scenario_changes), function(scn) {
    change <- scenario_changes[[scn]]
    data.table(
      date = dates,
      scenario = scn,
      value = start_value + c(0, cumsum(rep(change, horizon_months)))
    )
  }))
}

mf_plot_lines <- function(dt, x, y, color = "series", title, subtitle, y_label, colors = NULL) {
  p <- ggplot(dt, aes(x = .data[[x]], y = .data[[y]], color = .data[[color]])) +
    geom_line(linewidth = 0.85, na.rm = TRUE) +
    labs(title = title, subtitle = subtitle, x = NULL, y = y_label, color = NULL)
  if (!is.null(colors)) p <- p + scale_color_manual(values = colors)
  mf_theme(p)
}

mf_latest_text <- function(score) {
  if (!is.finite(score)) return("Latest: unavailable")
  label <- if (score >= 70) "elevated" else if (score >= 40) "moderate" else "low"
  paste0("Latest: ", label, " (", round(score), "/100)")
}
