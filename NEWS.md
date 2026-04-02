# investlabr news

## 0.0.4.2

- Refined the real-data Yahoo cross-asset event-board gallery example.
- Added interpolation-based NA handling after Yahoo OHLC load in the gallery script.
- Improved end-label spacing, datetime handling, and event callout styling in the gallery script.

## 0.0.4.1

- Added a real-data example gallery under `inst/gallery` to keep richer workflows outside the main README.
- Added executable gallery examples for FRED yield curves, Yahoo Finance candles, and a cross-asset event board.
- Updated `README.md` to point readers to the new gallery.

## 0.0.4

- Added a shared visualization style system with named styles and output contexts.
- Added reusable visualization helpers for palettes, themes, and common annotations.
- Refactored event, yield-curve, market-chart, backtest, and layout plots to accept `style` and `context`.
- Improved direct labeling and style-aware overlays for event, equity-curve, and market-chart outputs.
- Expanded README guidance for the new visualization styling layer.

## 0.0.3

- Fixed README examples so the portfolio mix workflow uses a feasible target return.
- Fixed the mocked candle-data example so generated OHLC values always satisfy candle-chart constraints.
- Added a Yahoo Finance plus `investdatar` end-to-end market-chart example to the README.
- Removed tracked local junk files from the repository working tree.

## 0.0.2

- Clarified package boundaries between `investlabr`, `investdatar`, and `strategyr`.
- Expanded package-level documentation for scope, architecture, and README usage examples.
- Added an end-to-end `investdatar` example to the README.
- Made README workflow examples more self-contained and representative of the current exported surface.

## 0.0.1

- Repositioned `investlabr` as a macro-financial and cross-asset research workflow package.
- Clarified package scope, boundaries, and design principles relative to `investdatar`.
- Reorganized the R source tree into `prep-*`, `factor-*`, `event-*`, `regime-*`, `sense-*`, `sim-*`, `viz-*`, and `brief-*` module families.
- Preserved legacy exported functions during the structural migration so downstream code can continue to work while the API evolves.
- Added package-level documentation for scope, architecture, and testing.
- Added a basic `testthat` harness with smoke tests for key package workflows.
