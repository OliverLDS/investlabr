# Real-Data Gallery

This gallery collects end-to-end `investlabr` usage cases built on real data accessed through `investdatar`.

The goal is to keep [README.md](../../README.md) concise while maintaining a growing library of reproducible research workflows.

## Conventions

- `investdatar` handles syncing and local access.
- `investlabr` handles transformation, comparison, and visualization.
- Examples are written as executable scripts for interactive use.
- Examples assume you already have the required data source credentials and local storage configured for `investdatar`.

## Available examples

- `real-data-fred-yield-curve.R`
  FRED Treasury series synced through `investdatar`, then plotted as a yield-curve comparison in `investlabr`.
- `real-data-yahoo-candles.R`
  Yahoo Finance OHLC data synced through `investdatar`, then rendered as a market chart in `investlabr`.
- `real-data-yahoo-cross-asset-event-board.R`
  Cross-asset Yahoo Finance event study with four panels: equity, bond, FX, and commodity.

## Usage

Run an example interactively from the package root:

```r
source("inst/gallery/real-data-fred-yield-curve.R")
source("inst/gallery/real-data-yahoo-candles.R")
source("inst/gallery/real-data-yahoo-cross-asset-event-board.R")
```

These scripts are intended to be edited for your own dates, tickers, and event labels.
