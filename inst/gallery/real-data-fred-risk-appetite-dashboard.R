library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

series_ids <- c("SP500", "NASDAQCOM", "VIXCLS", "BAMLH0A0HYM2", "BAMLH0A3HYC", "BAMLC0A4CBBB", "BAMLH0A1HYBB")

# Optional if you have not synced locally yet:
# invisible(lapply(series_ids, investdatar::sync_local_fred_data))

get_fred_local <- function(id, label = id) {
  dt <- data.table::as.data.table(investdatar::get_local_FRED_data(id))
  dt[, date := as.Date(date)]
  dt[, value := as.numeric(value)]
  dt[, series := label]
  dt[!is.na(value)]
}

cut_since <- function(dt, from = as.Date("2015-01-01")) {
  dt[date >= from]
}

equity <- rbindlist(list(
  get_fred_local("SP500", "S&P 500"),
  get_fred_local("NASDAQCOM", "NASDAQ")
), use.names = TRUE)
equity <- cut_since(equity)
first_dt <- max(
  equity[series == "S&P 500", min(date)],
  equity[series == "NASDAQ", min(date)]
)
equity <- equity[date >= first_dt]
equity[, base_val := value[date == first_dt][1], by = series]
equity[, idx := value / base_val * 100]

p_equity <- ggplot(equity, aes(x = date, y = idx, color = series)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = investlabr::viz_palette_get("cross_asset_color", "report", "discrete")[1:2]) +
  labs(
    title = "U.S. Equities",
    subtitle = "S&P 500 and NASDAQ rebased to 100",
    x = NULL,
    y = "Index (rebased = 100)"
  )
p_equity <- investlabr::viz_theme_apply(
  p_equity,
  style = "cross_asset_color",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

vix <- cut_since(get_fred_local("VIXCLS", "VIX"))
p_vix <- ggplot(vix, aes(x = date, y = value)) +
  geom_line(color = investlabr::viz_style_get("cross_asset_color", "report")$accent2, linewidth = 0.5) +
  labs(
    title = "Implied Volatility (VIX)",
    x = NULL,
    y = "VIX"
  )
p_vix <- investlabr::viz_theme_apply(
  p_vix,
  style = "cross_asset_color",
  context = "report",
  show_compiler = FALSE
)

credit <- rbindlist(list(
  get_fred_local("BAMLH0A0HYM2", "HY OAS"),
  get_fred_local("BAMLH0A3HYC", "CCC and below OAS"),
  get_fred_local("BAMLC0A4CBBB", "BBB OAS"),
  get_fred_local("BAMLH0A1HYBB", "BB OAS")
), use.names = TRUE)
credit <- cut_since(credit)

p_credit <- ggplot(credit, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = investlabr::viz_palette_get("cross_asset_color", "report", "discrete")[1:4]) +
  labs(
    title = "Corporate Credit Spreads",
    subtitle = "HY, BB, CCC, and BBB OAS",
    x = NULL,
    y = "Spread (%)"
  )
p_credit <- investlabr::viz_theme_apply(
  p_credit,
  style = "cross_asset_color",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_equity, p_vix, p_credit),
  n_rows = 3,
  n_cols = 1,
  title = "Risk Appetite Dashboard",
  bottom = "FRED inputs: SP500, NASDAQCOM, VIXCLS, BAMLH0A0HYM2, BAMLH0A3HYC, BAMLC0A4CBBB, BAMLH0A1HYBB.",
  style = "cross_asset_color",
  context = "report"
)
