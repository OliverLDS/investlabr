library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

.parse_args <- function(args) {
  defaults <- list(
    from_date = "2026-01-15",
    to_date = as.character(Sys.Date()),
    event_dates = "2026-02-28",
    event_labels = "Iran War"
  )
  if (length(args) == 0L) return(defaults)

  named <- grep("^--[^=]+=", args, value = TRUE)
  for (arg in named) {
    key <- sub("^--([^=]+)=.*$", "\\1", arg)
    val <- sub("^--[^=]+=", "", arg)
    if (key %in% names(defaults)) defaults[[key]] <- val
  }

  positional <- args[!grepl("^--", args)]
  if (length(positional) >= 1L) defaults$from_date <- positional[[1L]]
  if (length(positional) >= 2L) defaults$event_dates <- positional[[2L]]
  if (length(positional) >= 3L) defaults$event_labels <- positional[[3L]]
  if (length(positional) >= 4L) defaults$to_date <- positional[[4L]]
  defaults
}

.print_help <- function() {
  cat(
    paste(
      "Usage:",
      "Rscript /Users/oliver/Documents/2025/_2025-08-05_investlabr/investlabr/inst/gallery/real-data-yahoo-us-sector-11-event-board.R --event_dates=2026-02-28,2026-04-07 --event_labels=\"Iran War,Ceasefire\" --from_date=2026-01-01",
      "",
      "Named arguments:",
      "  --from_date=YYYY-MM-DD",
      "  --to_date=YYYY-MM-DD",
      "  --event_dates=YYYY-MM-DD[,YYYY-MM-DD...]",
      "  --event_labels=\"Label 1[,Label 2...]\"",
      sep = "\n"
    ),
    "\n"
  )
}

.split_arg <- function(x) {
  x <- trimws(unlist(strsplit(x, "[,|;]", fixed = FALSE)))
  x[nzchar(x)]
}

raw_args <- commandArgs(trailingOnly = TRUE)
if (any(raw_args %in% c("--help", "-h"))) {
  .print_help()
  quit(save = "no", status = 0)
}

args <- .parse_args(raw_args)
from_date <- as.Date(args$from_date)
to_date <- as.Date(args$to_date)
event_dates <- as.Date(.split_arg(args$event_dates))
event_labels <- .split_arg(args$event_labels)
if (length(event_dates) == 0L) stop("At least one event date is required.")
if (length(event_labels) == 0L) event_labels <- format(event_dates, "%Y-%m-%d")
if (length(event_labels) == 1L && length(event_dates) > 1L) {
  event_labels <- rep(event_labels, length(event_dates))
}
if (length(event_labels) != length(event_dates)) {
  stop("event_labels must have length 1 or the same length as event_dates.")
}

events_dt <- data.table(
  event_date = event_dates,
  event_label = event_labels,
  event_datetime = as.POSIXct(event_dates, tz = "UTC")
)

sector_meta <- data.table(
  symbol = c("XLK", "XLC", "XLY", "XLV", "XLP", "XLU", "XLI", "XLB", "XLE", "XLF", "XLRE"),
  short_label = c("Tech", "Comm", "Disc", "Health", "Staples", "Utilities", "Industrials", "Materials", "Energy", "Financials", "REITs"),
  panel = c(
    "Growth & Communication", "Growth & Communication", "Growth & Communication",
    "Defensive", "Defensive", "Defensive",
    "Cyclicals & Production", "Cyclicals & Production", "Cyclicals & Production",
    "Rates & Property", "Rates & Property"
  )
)

# This grouping is intentionally economic rather than numerically even:
# 1) Growth & Communication: tech, communication, discretionary
# 2) Defensive: health care, staples, utilities
# 3) Cyclicals & Production: industrials, materials, energy
# 4) Rates & Property: financials, real estate
# If you prefer a purely balanced 3-3-3-2 or a custom grouping, edit sector_meta$panel.

groups <- split(sector_meta$symbol, sector_meta$panel)
all_symbols <- sector_meta$symbol

# optionally syncing; by default should use local cache
# invisible(lapply(
#   all_symbols,
#   function(sym) investdatar::sync_local_quantmod_OHLC(
#     ticker = sym,
#     label = sym,
#     from = from_date,
#     to = to_date,
#     src = "yahoo"
#   )
# ))

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

.spread_last_labels <- function(dt, y_col, min_gap_frac = 0.05, jitter_frac = 0.018) {
  y_vals <- dt[[y_col]]
  if (length(y_vals) <= 1L) {
    dt[, label_y := y_vals]
    return(dt)
  }
  y_span <- diff(range(y_vals, na.rm = TRUE))
  if (!is.finite(y_span) || y_span <= 0) y_span <- max(abs(y_vals), na.rm = TRUE)
  if (!is.finite(y_span) || y_span <= 0) y_span <- 1
  min_gap <- y_span * min_gap_frac

  data.table::setorderv(dt, y_col)
  adjusted <- dt[[y_col]]
  centered_rank <- seq_along(adjusted) - mean(seq_along(adjusted))
  adjusted <- adjusted + centered_rank * y_span * jitter_frac
  for (i in 2:length(adjusted)) {
    if ((adjusted[i] - adjusted[i - 1L]) < min_gap) {
      adjusted[i] <- adjusted[i - 1L] + min_gap
    }
  }
  dt[, label_y := adjusted]
  dt
}

load_symbol_dt <- function(symbol) {
  dt <- investdatar::get_local_quantmod_OHLC(symbol, src = "yahoo")
  dt <- as.data.table(dt)[date >= from_date & date <= to_date]
  if (nrow(dt) == 0L) {
    stop("No Yahoo OHLC rows available for ", symbol, " between ", from_date, " and ", to_date, ".")
  }
  dt[, datetime := as.POSIXct(datetime, tz = "UTC")]
  numeric_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
  if (length(numeric_cols) > 0L) {
    dt[, (numeric_cols) := lapply(.SD, .interpolate_numeric), .SDcols = numeric_cols]
  }

  base_ref_date <- min(dt$date[dt$date >= from_date])
  base_ref_close <- dt[date == base_ref_date, close][1]
  dt[, `:=`(
    symbol = symbol,
    base_ref_date = base_ref_date,
    index100 = close / base_ref_close * 100
  )]
  dt
}

make_sector_panel <- function(symbols, panel_title) {
  dt_list <- lapply(symbols, load_symbol_dt)
  panel_dt <- rbindlist(dt_list, fill = TRUE)
  panel_dt <- merge(panel_dt, sector_meta[, .(symbol, short_label)], by = "symbol", all.x = TRUE)
  panel_dt[, label_text := short_label]

  last_labels <- panel_dt[!is.na(index100), .SD[.N], by = symbol]
  last_labels <- .spread_last_labels(last_labels, "index100", min_gap_frac = 0.10, jitter_frac = 0.05)

  label_pad <- max(1, ceiling(0.15 * uniqueN(panel_dt$datetime)))
  last_x <- max(panel_dt$datetime)
  x_limit_right <- last_x + as.difftime(label_pad, units = "days")

  y_range <- range(panel_dt$index100, last_labels$label_y, na.rm = TRUE)
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span <= 0) y_span <- max(abs(y_range), na.rm = TRUE)
  if (!is.finite(y_span) || y_span <= 0) y_span <- 1

  event_label_y <- y_range[2] + 0.06 * y_span
  last_labels[, datetime := last_x]

  daily_guides <- data.table(
    datetime = as.POSIXct(
      seq(as.Date(min(panel_dt$datetime)), as.Date(max(panel_dt$datetime)), by = "day"),
      tz = "UTC"
    )
  )
  panel_events <- copy(events_dt)
  panel_events[, y := event_label_y]

  p <- ggplot(panel_dt, aes(x = datetime, y = index100, color = symbol)) +
    geom_vline(
      data = daily_guides,
      aes(xintercept = datetime),
      inherit.aes = FALSE,
      linewidth = 0.12,
      alpha = 0.18,
      color = viz_style_get("macro_classic", "report")$muted
    ) +
    geom_line(linewidth = 0.9) +
    geom_vline(
      data = panel_events,
      aes(xintercept = event_datetime),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.7,
      alpha = 0.85,
      color = viz_style_get("macro_classic", "report")$muted
    ) +
    geom_label(
      data = panel_events,
      aes(x = event_datetime, y = y, label = event_label),
      inherit.aes = FALSE,
      size = 3,
      linewidth = 0.2,
      alpha = 0.95,
      fill = "white",
      color = viz_style_get("macro_classic", "report")$ink
    ) +
    geom_text(
      data = last_labels,
      aes(y = label_y, label = label_text),
      hjust = -0.1,
      vjust = 0.5,
      show.legend = FALSE,
      size = 3.0
    ) +
    labs(
      title = panel_title,
      subtitle = paste0("Indexed to 100 on ", from_date),
      x = "",
      y = ""
    ) +
    scale_x_datetime(
      limits = c(min(panel_dt$datetime), x_limit_right),
      date_breaks = "1 month",
      date_labels = "%Y-%m"
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

sector_plots <- lapply(names(groups), function(panel_name) {
  make_sector_panel(groups[[panel_name]], panel_name)
})

event_text <- paste(
  paste(format(event_dates, "%Y-%m-%d"), event_labels, sep = " = "),
  collapse = "; "
)

board <- investlabr::gen_grid_of_plots_with_labels(
  plots = sector_plots,
  n_rows = 2,
  n_cols = 2,
  title = "US Equity Sector Performance Board",
  bottom = paste0(
    "SPDR 11 sector ETFs indexed to 100 on ", from_date,
    ". Dashed lines mark: ", event_text, "."
  ),
  style = "macro_classic",
  context = "report"
)

# print(board)
# print can't show image in terminal; below is for output in terminal

dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
png("tmp/event-board.png", width = 1800, height = 1200, res = 150)
grid::grid.newpage()
grid::grid.draw(board)
dev.off()
system("open tmp/event-board.png")
