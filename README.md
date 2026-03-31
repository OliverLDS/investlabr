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
- `investlabr` thinks with the data

## Package Boundary

`investlabr` is not a raw data collection package and not a trading execution package.

Its role is the layer between:

1. data availability
2. research interpretation
3. presentation-ready output

This package is being expanded from a narrower strategy-evaluation toolkit into a broader research workflow package. Some current functions still reflect the earlier scope and will be migrated into a clearer module structure over time.

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

## Basic Usage

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

## Design Principles

- Keep the package focused on research workflows, not data acquisition.
- Prefer tidy, reusable transformation functions over one-off scripts.
- Return objects that are easy to reuse: `data.table`, tidy tables, `ggplot` objects, or small named lists.
- Separate data preparation, analytic logic, and visualization.
- Avoid hard-coded file paths, agent objects, or personal local directories.
- Make functions composable for dashboards and research notes.

## Testing

`investlabr` now includes a basic `testthat` setup for smoke coverage of package-level workflows.

```r
# install.packages("testthat")
testthat::test_dir("tests/testthat")
```

## License

MIT © 2025 Oliver Zhou
