library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

event_date <- as.Date("2026-02-28")
event_label <- "Iran War"
from_date <- "2026-01-15"
to_date <- "2026-04-01"

groups <- list(
  Equity = c("^GSPC", "^STOXX50E", "^N225", "^HSI"),
  Bond = c("ZT=F", "HYG", "LQD"),
  FX = c("DX-Y.NYB", "EURUSD=X", "GBPUSD=X"),
  Commodity = c("GC=F", "HG=F", "CL=F")
)

all_symbols <- unlist(groups, use.names = FALSE)
invisible(lapply(
  all_symbols,
  function(sym) investdatar::sync_local_quantmod_OHLC(
    ticker = sym,
    label = sym,
    from = from_date,
    to = to_date,
    src = "yahoo"
  )
))

.interpolate_numeric <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) return(x)
  idx <- seq_along(x)
  stats::approx(
    x = idx[!is.na(x)],
    y = x[!is.na(x)],
    xout = idx,
    method = "linear",
    rule = 2
  )$y
}

.spread_last_labels <- function(dt, y_col, min_gap_frac = 0.05) {
  y_vals <- dt[[y_col]]
  if (length(y_vals) <= 1L) return(dt)
  y_span <- diff(range(y_vals, na.rm = TRUE))
  if (!is.finite(y_span) || y_span <= 0) y_span <- max(abs(y_vals), na.rm = TRUE)
  if (!is.finite(y_span) || y_span <= 0) y_span <- 1
  min_gap <- y_span * min_gap_frac

  data.table::setorderv(dt, y_col)
  adjusted <- dt[[y_col]]
  for (i in 2:length(adjusted)) {
    if ((adjusted[i] - adjusted[i - 1L]) < min_gap) {
      adjusted[i] <- adjusted[i - 1L] + min_gap
    }
  }
  dt[, label_y := adjusted]
  dt
}

make_asset_panel <- function(symbols, panel_title) {
  dt_list <- lapply(symbols, function(symbol) {
    dt <- investdatar::get_local_quantmod_OHLC(symbol, src = "yahoo")
    dt <- data.table::as.data.table(dt)[date >= as.Date(from_date) & date <= as.Date(to_date)]
    dt[, datetime := as.POSIXct(datetime, tz = "UTC")]
    numeric_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
    if (length(numeric_cols) > 0L) {
      dt[, (numeric_cols) := lapply(.SD, .interpolate_numeric), .SDcols = numeric_cols]
    }
    base_ref_date <- max(dt$date[dt$date <= as.Date(from_date)])
    base_ref_close <- dt[date == base_ref_date, close][1]
    dt[, `:=`(
      symbol = symbol,
      base_ref_date = base_ref_date,
      index100 = close / base_ref_close * 100
    )]
    dt
  })

  panel_dt <- data.table::rbindlist(dt_list, fill = TRUE)
  panel_dt[, datetime := as.POSIXct(datetime, tz = "UTC")]
  last_labels <- panel_dt[!is.na(index100), .SD[.N], by = symbol]
  last_labels <- .spread_last_labels(last_labels, "index100", min_gap_frac = 0.10)
  label_pad <- max(1, ceiling(0.08 * uniqueN(panel_dt$datetime)))
  last_x <- max(panel_dt$datetime)
  x_limit_right <- last_x + as.difftime(label_pad, units = "days")
  y_range <- range(panel_dt$index100, last_labels$label_y, na.rm = TRUE)
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span <= 0) y_span <- max(abs(y_range), na.rm = TRUE)
  if (!is.finite(y_span) || y_span <= 0) y_span <- 1
  event_label_y <- y_range[2] + 0.06 * y_span
  last_labels[, datetime := last_x]

  p <- ggplot(panel_dt, aes(x = datetime, y = index100, color = symbol)) +
    geom_line(linewidth = 0.9) +
    geom_vline(
      xintercept = as.POSIXct(event_date, tz = "UTC"),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.7,
      alpha = 0.85,
      color = viz_style_get("macro_classic", "report")$muted
    ) +
    geom_label(
      data = data.table::data.table(
        datetime = as.POSIXct(event_date),
        y = event_label_y,
        label = event_label
      ),
      aes(x = datetime, y = y, label = label),
      inherit.aes = FALSE,
      size = 3,
      linewidth = 0.2,
      alpha = 0.95,
      fill = "white",
      color = viz_style_get("macro_classic", "report")$ink
    ) +
    geom_text(
      data = last_labels,
      aes(y = label_y, label = symbol),
      hjust = -0.1,
      vjust = 0.5,
      show.legend = FALSE,
      size = 3.2
    ) +
    labs(
      title = panel_title,
      subtitle = paste0("Indexed to 100 on ", from_date),
      x = "",
      y = ""
    ) +
    scale_x_datetime(
      limits = c(min(panel_dt$datetime), x_limit_right),
      date_labels = "%Y-%m-%d"
    )

  p <- investlabr::viz_theme_apply(
    p,
    style = "macro_classic",
    context = "report",
    legend_position = "none",
    show_compiler = FALSE
  )
  p + coord_cartesian(ylim = c(y_range[1], event_label_y + 0.04 * y_span))
}

equity_plot <- make_asset_panel(groups$Equity, "Equity")
bond_plot <- make_asset_panel(groups$Bond, "Bond")
fx_plot <- make_asset_panel(groups$FX, "FX")
commodity_plot <- make_asset_panel(groups$Commodity, "Commodity")

investlabr::gen_grid_of_plots_with_labels(
  plots = list(
    equity_plot, bond_plot,
    fx_plot, commodity_plot
  ),
  n_rows = 2,
  n_cols = 2,
  title = "Cross-Asset Performance After Feb 28, 2026",
  bottom = paste0(
    "Each series is indexed to 100 on ", from_date,
    ". The dashed line marks ", format(event_date, "%Y-%m-%d"), "."
  ),
  style = "macro_classic",
  context = "report"
)
