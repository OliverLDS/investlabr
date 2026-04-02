library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

event_date <- as.Date("2026-02-28")
event_label <- "Feb 28, 2026 (Iran War)"
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

make_asset_panel <- function(symbols, panel_title) {
  dt_list <- lapply(symbols, function(symbol) {
    dt <- investdatar::get_local_quantmod_OHLC(symbol, src = "yahoo")
    dt <- data.table::as.data.table(dt)[date >= as.Date(from_date) & date <= as.Date(to_date)]
    event_ref_date <- max(dt$date[dt$date <= event_date])
    event_ref_close <- dt[date == event_ref_date, close][1]
    dt[, `:=`(
      symbol = symbol,
      event_ref_date = event_ref_date,
      index100 = close / event_ref_close * 100
    )]
    dt
  })

  panel_dt <- data.table::rbindlist(dt_list, fill = TRUE)
  last_labels <- panel_dt[!is.na(index100), .SD[.N], by = symbol]

  p <- ggplot(panel_dt, aes(x = datetime, y = index100, color = symbol)) +
    geom_line(linewidth = 0.9) +
    geom_text(
      data = last_labels,
      aes(label = symbol),
      hjust = -0.1,
      vjust = 0.5,
      show.legend = FALSE,
      size = 3.2
    ) +
    labs(
      title = panel_title,
      subtitle = "Indexed to 100 on the last trading date on or before the event date",
      x = "",
      y = ""
    )

  p <- investlabr::viz_theme_apply(
    p,
    style = "macro_classic",
    context = "report",
    legend_position = "bottom"
  )

  investlabr::viz_annotate_event_lines(
    p,
    data.table::data.table(datetime = as.POSIXct(event_date), label = event_label),
    x_col = "datetime",
    label_col = "label",
    style = "macro_classic",
    context = "report"
  )
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
  bottom = "Each series is indexed to 100 at the last trading date on or before the event date.",
  style = "macro_classic",
  context = "report"
)
