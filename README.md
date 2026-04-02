# investlabr <img src="https://img.shields.io/badge/status-dev-blue.svg" align="right" />

## Research workflows for macro and cross-asset market analysis

`investlabr` is an R package for **macro-financial and cross-asset research workflows**.

It sits on top of data packages such as [`investdatar`](https://github.com/OliverLDS/investdatar) and provides reusable tools for:

- preparing research-ready datasets
- constructing macro and market factors
- analyzing events and regimes
- estimating sensitivities and exposures
- running scenario and structural-break simulations
- producing research-grade charts, dashboards, and briefing outputs

In simple terms:

- `investdatar` gets and stores the data
- `strategyr` turns strategy logic into executable actions
- `investlabr` thinks with the data and communicates the idea

## Package Boundary

`investlabr` is not a raw data collection package and not a trading execution package.

Its role is the layer between:

1. data availability
2. research interpretation
3. presentation-ready output

This package is being expanded from a narrower strategy-evaluation toolkit into a broader research workflow package. Some current functions still reflect the earlier scope and will be migrated into a clearer module structure over time.

`investlabr` is not the package that should decide live strategy actions. That role belongs more naturally to `strategyr`, where dynamic strategy logic and execution-oriented backtesting live. `investlabr` is the looser and more exploratory layer for visualizing market patterns, stress-testing ideas, and communicating investment thinking to a human audience.

## In Scope

- Research data preparation
- Macro and cross-asset diagnostics
- Event and regime analysis
- Sensitivity and exposure analysis
- Scenario and structural simulation
- Visualization and briefing output

Examples of likely function families:

- `prep_*`
- `factor_*`
- `event_*`
- `regime_*`
- `sense_*`
- `sim_*`
- `viz_*`
- `brief_*`

## Out of Scope

- Raw data downloading, syncing, and source registry management
- Agent memory, LLM orchestration, or workflow automation
- Trade execution, broker integration, and stateful backtesting infrastructure
- Portfolio accounting, holdings administration, and reconciliation

Those concerns belong in adjacent packages such as `investdatar`, dedicated orchestration wrappers, or trading-specific packages.

## Relationship to `strategyr`

`strategyr` and `investlabr` are related, but they solve different problems.

- `strategyr` is for timely investment decision-making
- `strategyr` owns realistic, path-dependent, execution-oriented backtesting
- `strategyr` should be able to support executable orders or action selection
- `investlabr` is for exploratory research, brainstorming, visualization, and human-facing idea sharing
- `investlabr` may keep lightweight or simplified backtesting when it helps explain a market pattern, but that is not its core identity

## Relationship to `investdatar`

`investdatar` owns:

- downloading
- syncing
- source metadata
- local storage
- source-specific parsing

`investlabr` owns:

- preparation of analysis-ready research tables
- factor and indicator construction
- event and regime overlays
- sensitivity estimation
- simulation and scenario comparison
- charting and briefing outputs

## Current Status

The repository currently includes legacy functions centered on event evaluation, strategy performance review, portfolio analysis, and plotting. Those functions remain useful, but the package identity is now broader:

> `investlabr` is an R package for reusable macro, rates, liquidity, and cross-asset research workflows, including factor construction, event/regime analysis, sensitivity estimation, scenario simulation, and research-grade visualization.

## Installation

```r
# install.packages("remotes")
remotes::install_github("OliverLDS/investlabr")
```

## Requirements

- R >= 3.5.0
- Core imports: `data.table`, `ggplot2`, `quadprog`, `Rcpp`
- Visualization helpers: `ggrepel`, `patchwork`, `gridExtra`, `scales`

## Repository Docs

- Package scope: [`PACKAGE_SCOPE.md`](./PACKAGE_SCOPE.md)
- Package architecture: [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- Release notes: [`NEWS.md`](./NEWS.md)
- Real-data gallery: [`inst/gallery/README.md`](./inst/gallery/README.md)

## Current Module Layout

The codebase is now organized by module family:

- `prep-*`: research data preparation scaffolds
- `factor-*`: reusable factor and diagnostic builders
- `event-*`: event-study style analytics
- `regime-*`: regime definitions and comparisons scaffolds
- `sense-*`: sensitivity and exposure scaffolds
- `sim-*`: simulation and legacy backtesting helpers
- `viz-*`: visualization and layout utilities
- `brief-*`: briefing-oriented plots and summaries

Some current exported functions still use older `eval_*` and `gen_*` names. Those remain for compatibility while the package migrates toward the new module naming convention.

## Visualization Styles

`investlabr` now includes a shared visualization style layer so charts can use a more professional and varied visual language without rewriting each plotting function.

Current named styles include:

- `research_note`
- `macro_classic`
- `terminal_risk`
- `cross_asset_color`
- `minimal_print`
- `presentation_bold`

Current contexts include:

- `report`
- `slide`
- `dashboard`

You can inspect or change the defaults with:

```r
library(investlabr)

viz_style_get()
viz_style_set(style = "macro_classic", context = "slide")
```

Most plotting functions now accept `style` and `context`, for example:

```r
gen_yield_curve_plot(curve_dt, style = "macro_classic", context = "slide")
gen_plot_event_tsline_cum_ret(evt, style = "presentation_bold", context = "report")
eval_strat_plot_tsline_eq(bt_res, style = "terminal_risk", context = "dashboard")
```

## Basic Usage

For larger real-data workflows, see the gallery in [`inst/gallery/README.md`](./inst/gallery/README.md). The README examples below stay relatively compact on purpose.

### Event workflow

```r
library(data.table)
library(investlabr)

DT <- data.table(
  datetime = seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = 120),
  close = cumprod(1 + rnorm(120, mean = 0.001, sd = 0.01)),
  event_policy = as.integer(seq_len(120) %% 20 == 0)
)

evt <- eval_event_performance(DT, "event_policy", H = 1L:10L)
gen_plot_event_tsline_cum_ret(evt)
```

### Event comparison workflow

```r
library(data.table)
library(investlabr)

DT <- data.table(
  datetime = seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = 150),
  close = cumprod(1 + rnorm(150, mean = 0.0005, sd = 0.01)),
  event_a = as.integer(seq_len(150) %% 25 == 0),
  event_b = as.integer(seq_len(150) %% 40 == 0)
)

evt_a <- eval_event_performance(DT, "event_a", H = 1L:15L)
evt_b <- eval_event_performance(DT, "event_b", H = 1L:15L)

gen_plot_comparing_events(
  evt_a, evt_b,
  first_event_label = "Event A",
  second_event_label = "Event B"
)
```

### Yield-curve workflow

```r
library(data.table)
library(investlabr)

yield_dt_list <- list(
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(5.1, 5.0)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(4.8, 4.7)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(4.5, 4.4)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(4.2, 4.1)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(4.0, 3.9)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(3.9, 3.8)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(3.8, 3.7)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(3.7, 3.6)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(3.6, 3.5)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(3.7, 3.6)),
  data.table(date = as.Date(c("2024-01-01", "2024-02-01")), value = c(3.8, 3.7))
)

curve_dt <- get_yield_data_DT(
  yield_dt_list,
  yield_dates = list(
    Now = as.Date("2024-02-01"),
    `One month ago` = as.Date("2024-01-01")
  )
)

gen_yield_curve_plot(curve_dt)
```

### Exploratory backtesting workflow

This is the lightweight and communication-oriented backtesting layer that remains in `investlabr`. More realistic and execution-grade strategy logic should live in `strategyr`.

```r
library(data.table)
library(investlabr)

DT <- data.table(
  datetime = seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = 120),
  open = 100 + cumsum(rnorm(120, 0.1, 1)),
  close = 100 + cumsum(rnorm(120, 0.1, 1)),
  pos_demo = rep(c(0, 1, 1, 0, -1, -1), length.out = 120)
)

attr(DT, "inst_id") <- "DEMO-ASSET"
attr(DT$pos_demo, "strat_name") <- "demo_strategy"
attr(DT$pos_demo, "strat_par") <- list(window = 20)

bt_res <- eval_strat_performance(DT, "pos_demo")

eval_strat_plot_tsline_eq(bt_res)
eval_strat_plot_scatter_maxdd_annret(bt_res)
```

If you want multiple rows for the scatter plot, bind several backtest results first:

```r
bt_res_list <- data.table::rbindlist(list(bt_res, bt_res))
eval_strat_plot_scatter_maxdd_annret(bt_res_list)
```

### Portfolio mix workflow

```r
library(data.table)
library(investlabr)

mk_bt_res <- function(asset_name, seed) {
  set.seed(seed)
  DT <- data.table(
    datetime = seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = 120),
    open = 100 + cumsum(rnorm(120, 0.1, 1)),
    close = 100 + cumsum(rnorm(120, 0.1, 1)),
    pos_demo = rep(c(0, 1, 1, 0, -1, -1), length.out = 120)
  )
  attr(DT, "inst_id") <- asset_name
  attr(DT$pos_demo, "strat_name") <- "demo_strategy"
  attr(DT$pos_demo, "strat_par") <- list(window = 20)
  eval_strat_performance(DT, "pos_demo")
}

bt_a <- mk_bt_res("DEMO-ASSET-A", seed = 1)
bt_b <- mk_bt_res("DEMO-ASSET-B", seed = 2)
bt_res_list <- data.table::rbindlist(list(bt_a, bt_b), fill = TRUE)

# Keep the target feasible for the sample inputs.
target_return <- min(bt_res_list$annual_return) * 0.8

weight_res <- get_optimal_weights(bt_res_list, target_return = target_return)
port_res <- eval_portfolio_performance(bt_res_list, target_return = target_return)

weight_res$optimal_weights_table
eval_strat_plot_tsline_eq(port_res)
```

### Market chart workflow

```r
library(data.table)
library(investlabr)

set.seed(1)
n <- 40
datetime <- seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = n)
open <- 100 + cumsum(rnorm(n, 0.1, 1))
close <- open + rnorm(n, 0, 1)
upper_wick <- abs(rnorm(n, 0.8, 0.3))
lower_wick <- abs(rnorm(n, 0.8, 0.3))

DT <- data.table(
  datetime = datetime,
  open = open,
  high = pmax(open, close) + upper_wick,
  low = pmin(open, close) - lower_wick,
  close = close
)

gen_candle_plots_with_sr_lines(
  DT,
  support_pts = c(95, 97.5),
  resistance_pts = c(108, 112)
)
```

### Multi-panel layout workflow

```r
library(data.table)
library(investlabr)

set.seed(1)
DT <- data.table(
  date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 150),
  close = 100 + cumsum(rnorm(150, 0.05, 1)),
  event_a = as.integer(seq_len(150) %% 25 == 0),
  event_b = as.integer(seq_len(150) %% 40 == 0)
)

p1 <- gen_plot_event_tsline_cum_ret(eval_event_performance(DT, "event_a", H = 1L:15L))
p2 <- gen_plot_event_tsline_cum_ret(eval_event_performance(DT, "event_b", H = 1L:15L))

gen_grid_of_plots_with_labels(
  plots = list(p1, p2),
  n_rows = 1,
  n_cols = 2,
  col_labs = c("Event A", "Event B"),
  title = "Event Comparison"
)
```

### Multi-panel layout workflow with real data

This example uses `investdatar` and Yahoo Finance data to build a 2 by 2 cross-asset event board. Equity, bond, FX, and commodity each get one subplot, and the symbols within each asset class are overlaid with different colors. Each series is indexed to 100 on the last trading day on or before the event date so the post-event paths are directly comparable inside each panel.

```r
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
  subtitle_date <- panel_dt[, min(event_ref_date)]
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
      subtitle = paste0("Indexed to 100 on ", format(subtitle_date, "%Y-%m-%d")),
      x = "",
      y = ""
    )

  p <- viz_theme_apply(p, style = "macro_classic", context = "report", legend_position = "bottom")
  p <- viz_annotate_event_lines(
    p,
    data.table::data.table(datetime = as.POSIXct(event_date), label = event_label),
    x_col = "datetime",
    label_col = "label",
    style = "macro_classic",
    context = "report"
  )
  p
}

equity_plot <- make_asset_panel(groups$Equity, "Equity")
bond_plot <- make_asset_panel(groups$Bond, "Bond")
fx_plot <- make_asset_panel(groups$FX, "FX")
commodity_plot <- make_asset_panel(groups$Commodity, "Commodity")

gen_grid_of_plots_with_labels(
  plots = list(
    equity_plot, bond_plot,
    fx_plot, commodity_plot
  ),
  n_rows = 2,
  n_cols = 2,
  title = "Cross-Asset Performance After Feb 28, 2026",
  bottom = "Each series is indexed to 100 at the last trading date on or before the event date."
)
```

### Working with `investdatar`

`investlabr` assumes data has already been retrieved or synchronized elsewhere. A typical workflow is:

```r
# library(investdatar)
# library(investlabr)
#
# fred_local <- investdatar::get_local_FRED_data("DGS10")
# curve_inputs <- ...
# research_tbl <- investlabr::get_yield_data_DT(curve_inputs, yield_dates = ...)
```

## End-To-End With `investdatar`

The example below shows the intended workflow boundary directly: `investdatar` reads the local FRED yield series, and `investlabr` converts those series into a research-ready yield-curve comparison chart.

```r
library(investdatar)
library(investlabr)

series_ids <- c(
  "DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3",
  "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"
)

# Optional if you have not synced these series locally yet:
invisible(lapply(series_ids, investdatar::sync_local_fred_data))

yield_dt_list <- lapply(series_ids, investdatar::get_local_FRED_data)

curve_dt <- investlabr::get_yield_data_DT(
  yield_dt_list = yield_dt_list,
  yield_dates = list(
    Now = Sys.Date(),
    `One month ago` = Sys.Date() - 30,
    `One year ago` = Sys.Date() - 365
  )
)

investlabr::gen_yield_curve_plot(
  curve_dt,
  selected_windows = c("Now", "One month ago", "One year ago")
)
```

### Yahoo Finance candles via `investdatar`

This example uses `investdatar` as the Yahoo Finance access layer and `investlabr` as the charting layer.

```r
library(data.table)
library(investdatar)
library(investlabr)

investdatar::sync_local_quantmod_OHLC(
  ticker = "SPY",
  from = "2024-01-01",
  to = "2024-12-31",
  src = "yahoo"
)

spy_dt <- investdatar::get_local_quantmod_OHLC("SPY", src = "yahoo")

investlabr::gen_candle_plots_with_sr_lines(
  data.table::as.data.table(spy_dt)[1:60, .(datetime, open, high, low, close)],
  support_pts = c(500, 520),
  resistance_pts = c(560, 580)
)
```

These examples show the intended package boundary:

- `investdatar` handles data syncing and local access
- `investlabr` turns those series into reusable research objects and charts
- `strategyr` is where dynamically adaptive, execution-grade strategy logic should live

## Design Principles

- Keep the package focused on research workflows, not data acquisition.
- Prefer tidy, reusable transformation functions over one-off scripts.
- Return objects that are easy to reuse: `data.table`, tidy tables, `ggplot` objects, or small named lists.
- Separate data preparation, analytic logic, and visualization.
- Avoid hard-coded file paths, agent objects, or personal local directories.
- Make functions composable for dashboards and research notes.
- Keep exploratory backtesting lightweight and communication-oriented; execution-grade strategy logic belongs in `strategyr`.

## Testing

`investlabr` now includes a basic `testthat` setup for smoke coverage of package-level workflows.

```r
# install.packages("testthat")
testthat::test_dir("tests/testthat")
```

## License

MIT © 2025 Oliver Zhou
