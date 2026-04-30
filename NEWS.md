# investlabr news

## 0.0.5

- Added an AlphaSync-oriented publishing layer for gallery outputs, including reusable plot-registry helpers, JSON manifest emission, YAML sidecars, and structured `plots/` and `thumbnails/` output folders under `inst/gallery/output/`.
- Added build scripts to render selected real-data gallery boards into publishing assets and to emit an AlphaSync-compatible `alphasync-plot-registry.json` manifest with controlled metadata and relative asset paths.
- Seeded the publishing registry with real macro and market boards plus style/context demo entries, and documented the sidecar and output-directory conventions in the gallery docs.
- Refined the Yahoo cross-asset event board so it behaves as a reusable renderable gallery script instead of opening a local image viewer.

## 0.0.4.16

- Added a full macro data forecast gallery family with executable FRED and Yahoo scripts for yield-curve fans, policy-rate paths, real-yield persistence, inflation/labor risk, liquidity and balance-sheet scenarios, cross-asset conditional fans, recession risk, landing scenarios, and regime transitions.
- Added shared gallery utilities for local macro forecast examples, including FRED/Yahoo loaders, forward-fan construction, scenario paths, bounded scores, and lightweight carry-forward helpers.
- Updated gallery documentation, the gallery index, and the promotion backlog so macro data forecast is documented as an implemented forward-looking research category.
- Refined the short-term FRED liquidity-tightness dashboard around reserve mechanics, rate-floor pressure, collateral/cash transmission, and an integrated tightness score.

## 0.0.4.15

- Expanded the macro data forecast gallery planning surface for forward-looking research scripts.
- Added and refined Yahoo event-board examples for cross-asset and US sector views, including multiple event markers, command-line arguments, annual/daily guides, and improved end-label handling.
- Refined several FRED macro monitor boards for cleaner storytelling: inflation/labor, FOMC plumbing, balance-sheet mirror, curve/real-yield, and macro-factor heatmap views.
- Expanded the Treasury nominal/real curve board with approximate implied forward-curve panels and clearer axis guides.

## 0.0.4.14

- Added a reusable candle-base plotting helper for strategy-explanation and support/resistance workflows.
- Added the `strategy_explain` visualization style for cleaner simulated rule-explanation boards.
- Expanded the gallery with simulated strategy-explanation scripts for Donchian, MACD, RSI, RSI-log-return, Bollinger, ATR breakout, ladder bounce/breakout, pair/ratio reversion, relative strength, curve steepener, trend pullback, EMA cross, and volatility-targeting examples.
- Updated gallery docs, the gallery index, the promotion backlog, and style-gallery assets to include the new strategy-explanation surface.

## 0.0.4.13

- Added a real-data FRED C&I lending monitor gallery script using local `investdatar` FRED cache data for TOTCI and selected SLOOS lending-condition series.
- Refined the C&I lending monitor board with long-history and recent weekly stock/growth panels, borrower-size facets for SLOOS standards/demand and loan-terms views, and annual guide lines on long-history panels.
- Added gallery maintenance docs: `GALLERY_INDEX.md`, `PROMOTION_BACKLOG.md`, and `_template-gallery-script.R` to organize future daily-analysis gallery additions and package-promotion candidates.
- Updated the gallery README to link the new maintenance docs and document the promotion convention for reusable gallery patterns.

## 0.0.4.12

- Refined several real-data `strategyr` gallery scripts around mined single-year cases for Bollinger, RSI, MACD, Donchian, and log-return RSI strategy examples.
- Added warmup-aware signal construction before trade-window backtesting in those gallery examples to reduce cold-start artifacts at the beginning of the displayed test period.
- Added `real-data-strategyr-rsi-logr-backtest.R` as a gallery example for `strategyr`'s log-return RSI mean-reversion strategy.
- Updated the Yahoo candle gallery to derive support and resistance from `strategyr` pivot, cycle, Fibonacci, and EMA-confluence logic, with quarterly x-axis labels, monthly guides, reader-facing title text, and compiler-aware captions.

## 0.0.4.11

- Extended `eval_strat_plot_tsline_eq()` to compare up to two evaluated strategy equity curves against one benchmark, with clearer title labels, semantic color names, and added volatility, Sharpe, and Sortino metrics.
- Added reusable `prep_backtest_result_from_equity()` and `prep_ratio_ohlc()` helpers so external `strategyr` backtest outputs and ratio proxies can be adapted into investlabr research plots.
- Expanded the real-data `strategyr` gallery with additional strategy examples, shared gallery utilities, absolute-path-safe sourcing, and explanatory narratives before plot output.
- Updated the curve-steepener gallery to use a one-trading-row-lagged Treasury 10Y-2Y signal and to compare the direct and contrarian versions against a buy-and-hold SHY/TLT ratio proxy.

## 0.0.4.10

- Removed deprecated `ggplot2::aes_string()` usage from visualization helpers.
- Added color-keyed metric annotations to `eval_strat_plot_tsline_eq()` so evaluated and benchmark lines are easier to connect with their summary metrics.
- Added five real-data `strategyr` backtest gallery examples using local Yahoo data, `strategyr::backtest_rcpp()`, and `investlabr::eval_strat_plot_tsline_eq()`.
- Reworked the settlement-mismatch simulation gallery around a breakout prediction-market leg plus short-straddle option leg, with clearer numerical assumptions and narrative framing.

## 0.0.4.9

- Added four new real-data gallery workflows: a Treasury nominal-versus-real curve board keyed to recent real-rate extremes, a Yahoo volatility-clustering board, a FRED rate-shock persistence board, and a Yahoo forward-fan board.
- Updated the gallery index and architecture notes to reflect the newer Treasury local-data workflow through `investdatar`.
- Refined the Treasury local-data gallery script with clearer reader-facing framing, denser guide lines, shorter date legends, and guarded tenor-level backfilling with warnings.

## 0.0.4.8

- Added research-facing `sim_*` time-series generators for AR, MA, ARCH, and GARCH-style data-generating processes.
- Standardized those simulation helpers around explicit validation, reproducible seeding, and `data.table` outputs suitable for downstream research workflows.
- Added package tests and generated documentation for the new simulation module.

## 0.0.4.7

- Fixed the export state for `gen_grid_of_plots_with_labels()` after the footer-wrap helper refactor so the function is available again from the package namespace.
- Added automatic wrapping for long `bottom` footer text in `gen_grid_of_plots_with_labels()`.
- Added regression coverage for wrapped grid-footer text.

## 0.0.4.6

- Expanded the gallery substantially with new real-data and simulation workflows covering iShares opportunity-set mapping, Fed plumbing and balance-sheet views, trade-conflict overlays, macro-factor sensitivity heatmaps, payoff mismatch scenarios, and regime simulations.
- Added a Treasury-source curve decomposition gallery board for nominal, real, and breakeven curves, and documented that Treasury XML curve ingestion should ultimately move into `investdatar` as a first-class source adapter.
- Updated architecture and gallery docs to reflect the broader gallery surface and the temporary boundary for direct-source Treasury examples.

## 0.0.4.5

- Clarified README guidance for quoting `plot_compiler_name` values that contain special YAML characters.
- Updated grid-based examples so subplot-level compiler captions are suppressed when the final output is assembled into a shared grid footer.
- Aligned the real-data cross-asset event-board gallery example with the same `show_compiler = FALSE` subplot behavior.

## 0.0.4.4

- Added six new visualization styles: `briefing_serif`, `institutional_blue`, `policy_memo`, `desk_monitor`, `client_slide`, and `newswire_print`.
- Added config-driven plot compiler caption support via `INVESTLABR_CONFIG` and `plot_compiler_name`.
- Extended compiler footer handling to higher-level grid and dashboard wrappers.
- Refreshed the style gallery scripts and static gallery images to cover the expanded style catalog.

## 0.0.4.3

- Added static style and context sample images under `inst/gallery/output`.
- Added executable style and context showcase scripts under `inst/gallery`.
- Updated gallery docs so users can inspect visualization styles without running the examples first.

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
