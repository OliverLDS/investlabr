# Gallery

This gallery collects both end-to-end real-data workflows and style-system showcases for `investlabr`.

The goal is to keep [README.md](../../README.md) concise while maintaining a growing library of reproducible research workflows.

## Conventions

- `investdatar` handles syncing and local access.
- `investlabr` handles transformation, comparison, and visualization.
- Examples are written as executable scripts for interactive use.
- Examples assume you already have the required data source credentials and local storage configured for `investdatar`.

## Style and context showcases

- `viz-style-gallery.R`
  Renders the same multi-series chart across all named visualization styles.
- `viz-context-gallery.R`
  Renders the same chart across `report`, `slide`, and `dashboard` contexts.

### Style samples

`research_note`

![research_note](./output/style-research_note.svg)

`macro_classic`

![macro_classic](./output/style-macro_classic.svg)

`terminal_risk`

![terminal_risk](./output/style-terminal_risk.svg)

`cross_asset_color`

![cross_asset_color](./output/style-cross_asset_color.svg)

`minimal_print`

![minimal_print](./output/style-minimal_print.svg)

`presentation_bold`

![presentation_bold](./output/style-presentation_bold.svg)

`briefing_serif`

![briefing_serif](./output/style-briefing_serif.svg)

`institutional_blue`

![institutional_blue](./output/style-institutional_blue.svg)

`policy_memo`

![policy_memo](./output/style-policy_memo.svg)

`desk_monitor`

![desk_monitor](./output/style-desk_monitor.svg)

`client_slide`

![client_slide](./output/style-client_slide.svg)

`newswire_print`

![newswire_print](./output/style-newswire_print.svg)

### Context samples

`report`

![report](./output/context-report.svg)

`slide`

![slide](./output/context-slide.svg)

`dashboard`

![dashboard](./output/context-dashboard.svg)

## Real-data workflows

- `real-data-fred-yield-curve.R`
  FRED Treasury series synced through `investdatar`, then plotted as a yield-curve comparison in `investlabr`.
- `real-data-yahoo-candles.R`
  Yahoo Finance OHLC data synced through `investdatar`, then rendered as a market chart in `investlabr`.
- `real-data-yahoo-cross-asset-event-board.R`
  Cross-asset Yahoo Finance event study with four panels: equity, bond, FX, and commodity.

## Usage

Run an example interactively from the package root:

```r
source("inst/gallery/viz-style-gallery.R")
source("inst/gallery/viz-context-gallery.R")
source("inst/gallery/real-data-fred-yield-curve.R")
source("inst/gallery/real-data-yahoo-candles.R")
source("inst/gallery/real-data-yahoo-cross-asset-event-board.R")
```

These scripts are intended to be edited for your own dates, tickers, and event labels.
